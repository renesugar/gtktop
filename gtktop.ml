(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2004-2011 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or any later version.                                             *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

module G = Gmylist

exception Error of string * int * int

class type display_box =
  object
    method coerce : GObj.widget
    method clear : unit
  end;;

class virtual gui_base =
  object
    method virtual bind : name:string -> callback:(unit -> unit) -> unit
    method virtual check_widgets : unit -> unit
    method virtual main : GWindow.window
    method virtual reparent : GObj.widget -> unit
    method virtual toplevel : GWindow.window
    method virtual vbox_code : GPack.box
    method virtual wb_execute : GButton.button
    method virtual wf_elements : GBin.frame
    method virtual wf_output : GBin.frame
    method virtual wf_display : GBin.frame
    method virtual wl_error : GMisc.label
    method virtual input_wscroll : GBin.scrolled_window
    method virtual output_wscroll : GBin.scrolled_window
    method virtual xml : Glade.glade_xml Gtk.obj
  end

class virtual ['a] param =
  object(self)
    inherit gui_base

    method s_color_user_code = "User code color"
    method s_color_output = "Output color"
    method s_color_error_background = "Error background color"
    method s_font_code = "Code font"
    method s_configuration = "Configuration"
    method s_save = "Save"
    method s_load_file = "Load file"
    method s_elements_frame_width = "Elements frame width"
    method s_display_frame_width = "Value frame width"
    method s_output_frame_height = "Output_frame_height"
    method s_error = "Error"

    method virtual about : unit -> unit
    method virtual window_title_prefix : string
    method virtual elts_columns :  (string * ('a -> string)) list
    method virtual display_elt : ('a -> unit) option
    method virtual execute : string -> (string * ('a * int) list)
    method virtual sourceview_language : string
    method virtual compare : 'a -> 'a -> int
    method virtual ini_file : string
    method display_box = (None : display_box option)
  end;;


let language_manager = Gtksv_utils.source_language_manager;;
Gtksv_utils.set_source_style_scheme
  (Gtksv_utils.read_style_scheme_selection ());;

let create_source_view ?editable ?packing param =
  let lang = language_manager#guess_language
    ~content_type: param#sourceview_language ()
  in
  let source_view =
    GSourceView2.source_view
      ?editable
      ~auto_indent:true
      ~insert_spaces_instead_of_tabs:true ~tab_width:2
      ~show_line_numbers:false
      ~smart_home_end: `ALWAYS
      ?packing
      ()
  in
  source_view#source_buffer#set_language lang;
  source_view#source_buffer#set_highlight_syntax true;
  (* set a style for bracket matching *)
  source_view#source_buffer#set_highlight_matching_brackets true;
  Gtksv_utils.register_source_buffer source_view#source_buffer ;
  Gtksv_utils.register_source_view source_view;
  Gtksv_utils.apply_sourceview_props source_view (Gtksv_utils.read_sourceview_props ()) ;
  source_view

let syntax_highlight_box param =
  let hb = GPack.hbox () in
  let lang = language_manager#guess_language
    ~content_type: param#sourceview_language () in
  match lang with
    None ->
      ignore(GMisc.label
       ~text: (Printf.sprintf "Syntax highlight for %s not available." param#sourceview_language)
         ~packing: hb#pack ())
      ;
      (Configwin.custom hb (fun () -> ()) false, fun () -> ())
  | Some lang ->
      let style_box = new Gtksv_utils.source_style_scheme_box () in
      hb#pack ~expand: true ~fill: true style_box#box;
      (Configwin.custom hb
       (fun () -> Gtksv_utils.store_style_scheme_selection style_box#scheme) true,
       (fun () -> Gtksv_utils.apply_source_style_scheme_to_registered_buffers
          (Gtksv_utils.source_style_scheme()))
      )
;;

let source_view_props_box () =
  let hb = GPack.hbox () in
  let p = Gtksv_utils.read_sourceview_props () in
  let box = new Gtksv_utils.sourceview_props_box
      Gtksv_utils.apply_sourceview_props_to_registered
  in
  box#set_props (Some p);
  hb#pack ~expand: true ~fill: true box#box;
  (Configwin.custom hb
     (fun () ->
       match box#props with
	 None -> ()
       | Some p -> Gtksv_utils.store_sourceview_props p
     ) true,
   (fun () ->
     Gtksv_utils.apply_sourceview_props_to_registered
       (Gtksv_utils.read_sourceview_props ()))
  )
;;

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)


type 'a element = { element : 'a ; pos : int };;

class ['a] elements (param : 'a param) (text : GText.view) =
  let columns = List.map
    (fun (title, f) -> Some title, G.String (fun e -> f e.element))
    param#elts_columns
  in
  object(self)
    inherit ['a element] G.plist
	`SINGLE
	columns
	(List.exists (function (t,_) -> t<>"") param#elts_columns)

    val mutable data = []
    method compare e1 e2 = param#compare e1.element e2.element

    method add_element (e : 'a element) =
      data <-
        (List.filter
         (fun ele -> self#compare ele e <> 0)
           data
        ) @ [e];
      self#update_data data

    method on_deselect _ =
      match param#display_box with
        None -> ()
      | Some b -> b#clear

    method on_select e =
      let it = text#buffer#get_iter_at_char e.pos in
      let it2 = text#buffer#get_iter_at_char (e.pos + 1) in
      ignore(text#scroll_to_iter it);
      text#buffer#select_range it it2;
      match param#display_elt with
        None -> ()
      | Some f -> f e.element

  end

(** {2 Options} *)

module O = Config_file

let op_ini = new O.group
let save_options file = op_ini#write file
let load_options file = op_ini#read file

let color_output = new O.string_cp ~group: op_ini
    ["colors" ; "output"]
    "Black" ""

let color_error = new O.string_cp ~group: op_ini
    ["colors" ; "error_background"]
    "Yellow" ""

let elements_frame_width = new O.int_cp ~group: op_ini
    ["elements_frame_width"] 150 ""

let display_frame_width = new O.int_cp ~group: op_ini
    ["display_frame_width"] 150 ""

let output_frame_height = new O.int_cp ~group: op_ini
    ["output_frame_height"] 300 ""


module C = Configwin

let edit_options param =
  let col_o = C.color ~f: color_output#set
      param#s_color_output color_output#get
  in
  let col_er = C.color ~f: color_error#set
      param#s_color_error_background color_error#get
  in
  let elements_frame_width = C.string
      ~f: (fun n -> try elements_frame_width#set (int_of_string n) with _ -> ())
      param#s_elements_frame_width (string_of_int elements_frame_width#get)
  in
  let display_frame_width = C.string
      ~f: (fun n -> try display_frame_width#set (int_of_string n) with _ -> ())
      param#s_display_frame_width (string_of_int display_frame_width#get)
  in
  let output_frame_height = C.string
      ~f: (fun n -> try output_frame_height#set (int_of_string n) with _ -> ())
      param#s_output_frame_height (string_of_int output_frame_height#get)
  in
  let (param_syntax, f_restore_syntax) = syntax_highlight_box param in
  let (param_svprops, f_restore_svprops) = source_view_props_box () in
  let sections =
    [
      C.Section ("Base",
		 [ col_o ; col_er ;
		   elements_frame_width ; display_frame_width ;
		   output_frame_height
		 ]) ;
      C.Section ("Source views", [param_svprops]) ;
      C.Section ("Syntax highlighting", [param_syntax]) ;
    ]
  in
  match C.get param#s_configuration sections with
    C.Return_ok -> save_options param#ini_file ; true
  | _ -> f_restore_syntax (); f_restore_svprops () ; false
;;

(** {2 The main box.} *)

class virtual gui param =
  let input_view = create_source_view param in
  let output_view = create_source_view ~editable: false param in
  object (self)
    val mutable file = (None : string option)

    val mutable elements =
      new elements param (output_view :> GText.view)

    (** The correctly evaluated phrases, in reverse order *)
    val mutable phrases = ([] : string list)

    method save_to_file f =
      try
        let oc = open_out f in
        let l = List.rev phrases in
        List.iter
          (fun s -> Printf.fprintf oc "%s\n" s)
          l;
        close_out oc
      with
        Sys_error s ->
          GToolbox.message_box param#s_error s
      | e ->
          GToolbox.message_box param#s_error (Printexc.to_string e)

    method save_as () =
      match GToolbox.select_file param#s_save () with
        None -> ()
      |	Some f ->
          file <- Some f;
          param#main#set_title (param#window_title_prefix^": "^f);
          self#save_to_file f

    method save () =
      match file with
        None -> self#save_as ()
      |	Some f -> self#save_to_file f


    method set_error_message = function
      None -> param#wl_error#set_text ""; param#wl_error#misc#hide ()
    |	Some s ->
	  param#wl_error#set_text s ;
	  param#wl_error#misc#show ()

    method execute () =
      let b = input_view#buffer in
      try
        try
          self#set_error_message None;
          (match param#display_box with None -> () | Some b -> b#clear) ;
          let code = b#get_text ~start: b#start_iter ~stop: b#end_iter () in
          let (output, results) = param#execute code in
          phrases <- code :: phrases;
          output_view#buffer#place_cursor ~where: output_view#buffer#end_iter;
          List.iter
            (fun (e, pos) ->
               elements#add_element
                 {element = e; pos = output_view#buffer#char_count + pos})
            results;
          output_view#buffer#insert (code^"\n");
          let tag = output_view#buffer#create_tag [`FOREGROUND color_output#get] in
          output_view#buffer#insert ~tags: [tag] output;
          input_view#buffer#delete ~start: b#start_iter ~stop: b#end_iter;
        with
          Error (message, st, en) ->
            let start = b#get_iter_at_char st in
            let stop = b#get_iter_at_char en in
            let tag = b#create_tag [`BACKGROUND color_error#get] in
            b#apply_tag tag ~start ~stop;
            self#set_error_message (Some message)
      with
        e ->
          let s = Printexc.to_string e in
          GToolbox.message_box param#s_error s

    method execute_file f =
      try
        let b = input_view#buffer in
        let code = string_of_file f in
        b#delete ~start: b#start_iter ~stop: b#end_iter;
        b#insert code;
        self#execute ()
      with
      | Sys_error s
      | Failure s ->
          GToolbox.message_box param#s_error s

    method load_file f =
      self#execute_file f;
      file <- Some f;
      param#main#set_title (param#window_title_prefix^": "^f)

    method ask_load_file () =
      match GToolbox.select_file param#s_load_file () with
        None -> ()
      |	Some f -> self#load_file f

    method set_widths () =
      param#wf_elements#misc#set_size_request ~width: elements_frame_width#get ();
      param#wf_display#misc#set_size_request ~width:
        (match param#display_elt with None -> 0 | Some _ -> display_frame_width#get) () ;
      param#wf_output#misc#set_size_request ~height: output_frame_height#get ();

    method edit_options () =
      if edit_options param then
        (self#set_widths ())

    method quit () = param#main#destroy ()

    initializer
      load_options param#ini_file;
      save_options param#ini_file;
      param#input_wscroll#add input_view#coerce;
      param#output_wscroll#add output_view#coerce;
      param#main#set_title param#window_title_prefix;
      self#set_error_message None;
      self#set_widths ();
      ignore(param#main#connect#destroy GMain.Main.quit);
      ignore(param#wb_execute#connect#clicked self#execute);

      let handlers =
        [ "on_quit_activate", `Simple self#quit;
          "on_preferences_activate", `Simple self#edit_options;
          "on_about_activate",`Simple param#about;
          "on_open_activate", `Simple self#ask_load_file;
          "on_save_activate", `Simple self#save ;
          "on_save_as_activate", `Simple self#save_as ;
        ]
      in
      (* Finalize GUI *)
      Glade.bind_handlers ~extra:handlers ~warn:true param#xml;

      Okey.add input_view
        ~mods: [`CONTROL] GdkKeysyms._Return self#execute;

      param#wf_elements#add elements#box#coerce;
      (match param#display_box with None -> () | Some b -> param#wf_display#add b#coerce);

      (
       match file with
         None -> ()
       | Some f -> param#main#set_title (param#window_title_prefix^": "^f)
      );

      param#main#show ()
  end