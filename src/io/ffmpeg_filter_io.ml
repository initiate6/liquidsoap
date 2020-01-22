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

(** Connect sources to FFmpeg filters. *)

module ToFrame =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

module FromFrame =
  Swresample.Make (Swresample.Frame) (Swresample.FltPlanarBigArray)

module Scaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)
module Generator = Generator.From_audio_video

(** From the script perspective, the operator sending data to a filter graph
  * is an output. *)
class audio_output ~name ~kind val_source =
  let channels = Frame.((type_of_kind kind).audio) in
  let channels_layout =
    try Avutil.Channel_layout.get_default channels
    with Not_found ->
      failwith
        "ffmpeg filter: could not find a default channel configuration for \
         this number of channels.."
  in
  let samplerate = Frame.audio_of_seconds 1. in
  let converter =
    ToFrame.create ~out_sample_format:`Dbl channels_layout samplerate
      channels_layout samplerate
  in
  let noop () = () in
  object
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~content_kind:kind ~name
          ~output_kind:"ffmpeg.filter.input" val_source true

    val mutable input = fun _ -> assert false

    method set_input fn = input <- fn

    method output_start = ()

    method output_stop = ()

    method output_reset = ()

    method output_send memo =
      let pcm = AFrame.content memo 0 in
      let aframe = ToFrame.convert converter pcm in
      input aframe
  end

type in_config = { format : Avutil.Sample_format.t; rate : int; channels : int }

(* Same thing here. *)
class audio_input ~kind =
  let channels_layout channels =
    try Avutil.Channel_layout.get_default channels
    with Not_found ->
      failwith
        "ffmpeg filter: could not find a default channel configuration for \
         this number of channels.."
  in
  let out_channels = Frame.((type_of_kind kind).audio) in
  let out_channels_layout = channels_layout out_channels in
  let out_samplerate = Frame.audio_of_seconds 1. in
  let mk_converter { format; rate; channels } =
    FromFrame.create ~in_sample_format:format ~out_sample_format:`Dbl
      (channels_layout channels) rate out_channels_layout out_samplerate
  in
  let config =
    { format = `Dbl; rate = out_samplerate; channels = out_channels }
  in
  let generator = Generator.create `Audio in
  object (self)
    inherit Source.source kind ~name:"ffmpeg.filter.output"

    val mutable in_config = config

    val mutable converter = mk_converter config

    val mutable output = fun _ -> assert false

    method set_output fn = output <- fn

    method self_sync = false

    method stype = Source.Fallible

    method remaining = Generator.remaining generator

    method private get_output_frame =
      let f = output () in
      let config =
        {
          format = Avutil.Audio.frame_get_sample_format f;
          rate = Avutil.Audio.frame_get_sample_rate f;
          channels = Avutil.Audio.frame_get_channels f;
        }
      in
      if config <> in_config then begin
        self#log#important "format change detected!";
        in_config <- config;
        converter <- mk_converter config
      end;
      f

    method private flush_buffer =
      let rec f () =
        try
          let pcm = FromFrame.convert converter self#get_output_frame in
          Generator.put_audio generator pcm 0 (Audio.length pcm);
          f ()
        with Avutil.Error `Eagain -> ()
      in
      f ()

    method is_ready =
      self#flush_buffer;
      Generator.length generator > 0

    method private get_frame frame =
      self#flush_buffer;
      Generator.fill generator frame;
      if Frame.is_partial frame && Generator.length generator = 0 then
        self#log#important "Buffer emptied..."

    method abort_track = ()
  end
