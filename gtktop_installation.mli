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

val glade_dir : string
val software_version : string
val glade_file : string
class default_main_gui :
  ?file:string ->
  unit ->
  object
    val menu_file : GMenu.menu
    val menu_help : GMenu.menu
    val menuitem_file : GMenu.menu_item
    val menuitem_help : GMenu.menu_item
    val input_wscroll : GBin.scrolled_window
    val main : GWindow.window
    val output_wscroll : GBin.scrolled_window
    val toplevel : GWindow.window
    val vbox : GPack.box
    val vbox_code : GPack.box
    val wb_execute : GButton.button
    val wf_display : GBin.frame
    val wf_elements : GBin.frame
    val wf_output : GBin.frame
    val wl_error : GMisc.label
    val xml : Glade.glade_xml Gtk.obj
    method menu_file : GMenu.menu
    method menu_help : GMenu.menu
    method menuitem_file : GMenu.menu_item
    method menuitem_help : GMenu.menu_item
    method bind : name:string -> callback:(unit -> unit) -> unit
    method check_widgets : unit -> unit
    method input_wscroll : GBin.scrolled_window
    method main : GWindow.window
    method output_wscroll : GBin.scrolled_window
    method reparent : GObj.widget -> unit
    method toplevel : GWindow.window
    method vbox : GPack.box
    method vbox_code : GPack.box
    method wb_execute : GButton.button
    method wf_display : GBin.frame
    method wf_elements : GBin.frame
    method wf_output : GBin.frame
    method wl_error : GMisc.label
    method xml : Glade.glade_xml Gtk.obj
  end
