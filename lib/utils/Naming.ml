(* I/O file utils *)
open Base
open Stdio
open Config

let tmp_file_index = ref 0

let tmp_file (base : string) (extension : string) : string =
  let i = !tmp_file_index in
  let hd = Caml.Filename.remove_extension (Caml.Filename.basename !master_file) in
  tmp_file_index := i + 1;
  tmp_dir ^ "/" ^ hd ^ "_" ^ base ^ Int.to_string i ^ extension

let copy_file from_filename to_filename =
  let oc = Out_channel.create to_filename in
  let ic = In_channel.create from_filename in
  try
    while true do
      let line = In_channel.input_line_exn ic in
      Out_channel.output_string oc (line ^ "\n")
    done
  with End_of_file ->
    In_channel.close ic;
    Out_channel.close oc

let remove_in_dir dirname =
  try
    if Stdlib.Sys.is_directory dirname then
      let filenames = Stdlib.Sys.readdir dirname in
      let complete_fn = Array.map ~f:(fun s -> dirname ^ s) filenames in
      Array.iter
        ~f:(fun filename ->
          if Stdlib.Sys.is_directory filename then () else Stdlib.Sys.remove filename)
        complete_fn
    else raise (Sys_error "Not a directory name")
  with Sys_error s -> eprintf "Remove_in_dir : %s" s

(* Other naming conventions *)
let new_accumulator = "_acc"

let new_accumulator_struct = "$accum"

let new_struct_field = "m"

let new_struct_list_field = "l"

let struct_equality_name s = s ^ "-equal?"

(* File extensions, *)
let ext_racket = ".rkt"
