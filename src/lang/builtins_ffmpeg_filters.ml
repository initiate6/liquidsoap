(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Lang_builtins

type 'a input = 'a Avutil.frame -> unit
type 'a output = unit -> 'a Avutil.frame
type 'a setter = 'a -> unit
type 'a entries = (string, 'a setter) Hashtbl.t
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) Avfilter.av

type outputs =
  ([ `Audio ] output entries, [ `Video ] output entries) Avfilter.av

type graph = {
  config : Avfilter.config;
  entries : (inputs, outputs) Avfilter.io;
}

module Graph = Lang.MkAbstract (struct
  type content = graph

  let name = "ffmpeg.filter.graph"
end)

module Audio = Lang.MkAbstract (struct
  type content =
    [ `Input of ([ `Attached ], [ `Audio ], [ `Input ]) Avfilter.pad
    | `Output of ([ `Attached ], [ `Audio ], [ `Output ]) Avfilter.pad ]

  let name = "ffmpeg.filter.audio"
end)

module Video = Lang.MkAbstract (struct
  type content =
    [ `Input of ([ `Attached ], [ `Video ], [ `Input ]) Avfilter.pad
    | `Output of ([ `Attached ], [ `Video ], [ `Output ]) Avfilter.pad ]

  let name = "ffmpeg.filter.video"
end)

let uniq_name =
  let names = Hashtbl.create 10 in
  let name_idx name =
    match Hashtbl.find_opt names name with
      | Some x ->
          Hashtbl.replace names name (x + 1);
          x
      | None ->
          Hashtbl.add names name 1;
          0
  in
  fun name -> Printf.sprintf "%s_%d" name (name_idx name)

let apply_filter ~filter p =
  Avfilter.(
    let { config; _ } = Graph.of_value (Lang.assoc "" 1 p) in
    let name = uniq_name filter.name in
    let filter = attach ~name filter config in
    let audio_inputs_c = List.length filter.io.inputs.audio in
    List.iteri
      (fun idx input ->
        let output =
          match Audio.of_value (Lang.assoc "" (idx + 1) p) with
            | `Output output -> output
            | _ -> assert false
        in
        link output input)
      filter.io.inputs.audio;
    List.iteri
      (fun idx input ->
        let output =
          match Video.of_value (Lang.assoc "" (audio_inputs_c + idx + 1) p) with
            | `Output output -> output
            | _ -> assert false
        in
        link output input)
      filter.io.inputs.video;
    Lang.tuple
      ( List.map (fun p -> Audio.to_value (`Output p)) filter.io.outputs.audio
      @ List.map (fun p -> Video.to_value (`Output p)) filter.io.outputs.video
      ))

let () =
  Avfilter.(
    let mk_av_t { audio; video } =
      let audio = List.map (fun _ -> Audio.t) audio in
      let video = List.map (fun _ -> Video.t) video in
      audio @ video
    in
    List.iter
      (fun ({ name; description; io } as filter) ->
        let input_t =
          [("", Graph.t, None, None)]
          @ List.map (fun t -> ("", t, None, None)) (mk_av_t io.inputs)
        in
        let output_t = Lang.tuple_t (mk_av_t io.outputs) in
        add_builtin ~cat:Liq ("ffmpeg.filter." ^ name)
          ~descr:("Ffmpeg filter: " ^ description)
          input_t output_t (apply_filter ~filter))
      filters)

let get_filter =
  let filters = Hashtbl.create 10 in
  fun ~source name ->
    match Hashtbl.find_opt filters name with
      | Some f -> f
      | None -> (
          match List.find_opt (fun f -> f.Avfilter.name = name) source with
            | Some f ->
                Hashtbl.add filters name f;
                f
            | None -> failwith ("Could not find ffmpeg filter: " ^ name) )

let () =
  let audio_t = Lang.(source_t (kind_type_of_kind_format (audio_n 2))) in
  let kind = Frame.{ audio = mul_of_int 2; video = Zero; midi = Zero } in

  add_builtin ~cat:Liq "ffmpeg.filter.audio.input"
    ~descr:"Attach an audio source to a filter's input"
    [("", Graph.t, None, None); ("", audio_t, None, None)] Audio.t (fun p ->
      let graph = Graph.of_value (Lang.assoc "" 1 p) in
      let source_val = Lang.assoc "" 2 p in
      let name = uniq_name "abuffer" in
      let abuffer = get_filter ~source:Avfilter.buffers "abuffer" in
      let abuffer = Avfilter.attach ~name abuffer graph.config in
      let s = Ffmpeg_filter_io.(new audio_output ~name ~kind source_val) in
      Avfilter.(Hashtbl.add graph.entries.inputs.audio name s#set_input);
      Audio.to_value (`Output (List.hd Avfilter.(abuffer.io.outputs.audio))));

  add_builtin ~cat:Liq "ffmpeg.filter.audio.output"
    ~descr:"Return an audio source from a filter's output"
    [("", Graph.t, None, None); ("", Audio.t, None, None)] audio_t (fun p ->
      let graph = Graph.of_value (Lang.assoc "" 1 p) in
      let pad =
        match Audio.of_value (Lang.assoc "" 2 p) with
          | `Output p -> p
          | _ -> assert false
      in
      let name = uniq_name "abuffersink" in
      let abuffersink = get_filter ~source:Avfilter.sinks "abuffersink" in
      let abuffersink = Avfilter.attach ~name abuffersink graph.config in
      Avfilter.(link pad (List.hd abuffersink.io.inputs.audio));
      let s = Ffmpeg_filter_io.(new audio_input ~kind) in
      Avfilter.(Hashtbl.add graph.entries.outputs.audio name s#set_output);
      Lang.source (s :> Source.source));

  let video_t = Lang.(source_t (kind_type_of_kind_format video_only)) in
  add_builtin ~cat:Liq "ffmpeg.filter.video.input"
    ~descr:"Attach a video source to a filter's input"
    [("", Graph.t, None, None); ("", video_t, None, None)] Video.t (fun _ ->
      assert false);

  add_builtin ~cat:Liq "ffmpeg.filter.video.output"
    ~descr:"Return a video source from a filter's output"
    [("", Graph.t, None, None); ("", Video.t, None, None)] video_t (fun _ ->
      assert false)

let () =
  add_builtin "ffmpeg.filter.create" ~cat:Liq
    ~descr:"Configure and launch a filter graph"
    [("", Lang.fun_t [(false, "", Graph.t)] Lang.unit_t, None, None)]
    Lang.unit_t
    (fun _ -> assert false)
