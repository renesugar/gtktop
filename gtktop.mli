(*********************************************************************************)
(*                Gtktop                                                         *)
(*                                                                               *)
(*    Copyright (C) 2005-2012 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Generic interface to create GUI for interpreters. *)

(** The exception to use to report parse or execution error in input code.
   In [Error (message, start, stop)]:
   - the two integers are start and stop character positions in the parsed string,
   - [message] is the error message. *)
exception Error of string * int * int

(** {2 Parametrizing the interface}

The elements box is the list of defined elements, on the left in
the default (glade) interface.

The display box is the box used to display the value associated to the selected
element.

The output view is the sourceview where output of evaluation is printed
along with a copy of the input code.

The input view is where the use types in its code.
*)

(** What the display box must provide. *)
class type display_box =
  object method clear : unit method coerce : GObj.widget end

(** What the gui must provide. These are provided by default by the
{!Custop_installation.default_main_gui} class, built using a glade file
(from which the {!Custop_base} module is generated). *)
class virtual gui_base :
  object
    method virtual bind : name:string -> callback:(unit -> unit) -> unit
    method virtual check_widgets : unit -> unit
    method virtual input_wscroll : GBin.scrolled_window
    method virtual main : GWindow.window
    method virtual output_wscroll : GBin.scrolled_window
    method virtual reparent : GObj.widget -> unit
    method virtual toplevel : GWindow.window
    method virtual vbox_code : GPack.box
    method virtual wb_execute : GButton.button
    method virtual wf_display : GBin.frame
    method virtual wf_elements : GBin.frame
    method virtual wf_output : GBin.frame
    method virtual wl_error : GMisc.label
    method virtual xml : Glade.glade_xml Gtk.obj
  end

(** What the user of this library must provide.
   ['a] represents the type of the elements which can be defined
   by the code of the user.*)
class virtual ['a] param :
  object
    (** Developer must provide the gui elements. *)
    inherit gui_base

    (** This method should display a "About ..." window. *)
    method virtual about : unit -> unit

    (** This method will be used to compare two elements. *)
    method virtual compare : 'a -> 'a -> int

    (** Return the optional display box. Default is [None]. *)
    method display_box : display_box option

    (** This method will be called to display the selected element.
    It should be [Some ...] if there is a display box.*)
    method virtual display_elt : ('a -> unit) option

    (** This method returns a description of the columns to display
         in the list of elements, and for each column a function to
         get a string from an element. *)
    method virtual elts_columns : (string * ('a -> string)) list

    (** This method is called to execute the user's code. It returns
         the output (result) string and for each defined element
         a pair [(element, position of first character)]. *)
    method virtual execute : string -> string * ('a * int) list

    (** This method returns the absolute name of the file to use
         to store options.*)
    method virtual ini_file : string

    (** This method returns the mime type of the sourceview language to
         use for syntax highlighting. *)
    method virtual sourceview_language : string

    (** This method returns the prefix string to use in the window title. *)
    method virtual window_title_prefix : string

    (** {3 Methods returning strings used in interface}
    These can be overriden. *)

    method s_color_error_background : string
    method s_color_output : string
    method s_color_user_code : string
    method s_configuration : string
    method s_display_frame_width : string
    method s_elements_frame_width : string
    method s_error : string
    method s_font_code : string
    method s_load_file : string
    method s_output_frame_height : string
    method s_save : string
  end;;

(** {2 Utilities} *)

(** This function can be used to create a {!Configwin} box to configure
   the style scheme used in sourceview. *)
val syntax_highlight_box : 'a param ->  Configwin.parameter_kind * (unit -> unit)

(** This function can be used to create a {!Configwin} box to configure
     the common preferences of the sourceview widgets. *)
val source_view_props_box : unit -> Configwin.parameter_kind * (unit -> unit)

(** [string_of_file file] returns the contents of the whole [file] as a string. *)
val string_of_file : string -> string

(** {2 The list of elements} *)

(** To represent an element and its position in the output view, so that
     we can automatically scroll to the definition of the element when
     it is selected in the element list. *)
type 'a element = { element : 'a; pos : int; }

(** The elements box. *)
class ['a] elements :
  'a param ->
  GText.view ->
  object
    inherit ['a element] Gmylist.plist
    val mutable data : 'a element list
    method add_element : 'a element -> unit
    method compare : 'a element -> 'a element -> int
  end

(** {2 Options} *)

(** The group of options. See {!Config_file}. *)
val op_ini : Config_file.group

(** [save_options file] saves the options in the given [file]. *)
val save_options : string -> unit

(** [load_file] loads the options for the given [file]. *)
val load_options : string -> unit

(** The option for the color of (result) output. *)
val color_output : Config_file.string_cp

(** The option for the color of error in input view. *)
val color_error : Config_file.string_cp

(** The option for the width of the elements box. *)
val elements_frame_width : Config_file.int_cp

(** The option for the width of the display box. *)
val display_frame_width : Config_file.int_cp

(** The option for the height of the output view. *)
val output_frame_height : Config_file.int_cp

(** This function opens a window to make the user edit the default options above.
     @return [true] if options were modified. *)
val edit_options : 'a param -> bool

(** {2 The main window}

Create an instance of this class with a [param] object and the main window
will appear. [GMain.Main.main ()] is not called, so you have to call it
yourself.
*)

class virtual gui :
  'a param ->
  object
    val mutable elements : 'a elements
    val mutable file : string option
    val mutable phrases : string list
    method ask_load_file : unit -> unit
    method edit_options : unit -> unit
    method execute : unit -> unit
    method execute_file : string -> unit
    method load_file : string -> unit
    method quit : unit -> unit
    method save : unit -> unit
    method save_as : unit -> unit
    method save_to_file : string -> unit
    method set_error_message : string option -> unit
    method set_widths : unit -> unit
  end
