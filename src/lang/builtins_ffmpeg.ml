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
open FFmpeg

module Graph = Lang.MkAbstract (struct
  type content = Avfilter.config

  let name = "ffmpeg.filter.graph"
end)

module Audio = Lang.MkAbstract (struct
  type content = Avfilter.config

  let name = "ffmpeg.filter.audio"
end)

module Video = Lang.MkAbstract (struct
  type content = Avfilter.config

  let name = "ffmpeg.filter.video"
end)

let () =
  Avfilter.(
    let mk_av_t { audio; video } =
      let audio = List.map (fun _ -> Audio.t) audio in
      let video = List.map (fun _ -> Video.t) video in
      audio @ video
    in
    List.iter
      (fun { name; description; io } ->
        let input_t =
          List.map (fun t -> ("", t, None, None)) (mk_av_t io.inputs)
        in
        let output_t = Lang.tuple_t (mk_av_t io.outputs) in
        add_builtin ~cat:Liq ("ffmpeg.filter." ^ name)
          ~descr:("Ffmpeg filter: " ^ description) input_t output_t (fun _ ->
            assert false))
      filters)

let () =
  let audio_t = Lang.(source_t (kind_type_of_kind_format (audio_n 1))) in
  add_builtin ~cat:Liq "ffmpeg.filter.audio.input"
    ~descr:"Attach an audio source to a filter's input"
    [("", Graph.t, None, None); ("", audio_t, None, None)] Audio.t (fun _ ->
      assert false);
  add_builtin ~cat:Liq "ffmpeg.filter.audio.output"
    ~descr:"Return an audio source from a filter's output"
    [("", Graph.t, None, None); ("", Audio.t, None, None)] audio_t (fun _ ->
      assert false);
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
  let ret_t = Lang.univ_t () in
  add_builtin "ffmpeg.filter.create" ~cat:Liq
    ~descr:"Configure and launch a filter graph"
    [("", Lang.fun_t [(false, "", Graph.t)] ret_t, None, None)]
    ret_t
    (fun _ -> assert false)
