open! Core_kernel.Std

type t =
| Application_cursor_keys (* DECCKM) *)
| Designate_USASCII (* for character sets G0-G3 (DECANM), and set VT100 mode. *)
| N_132_column_mode (* (DECCOLM) *)
| Smooth_slow_scroll (* (DECSCLM) *)
| Reverse_video (* (DECSCNM) *)
| Origin_mode (* (DECOM) *)
| Wraparound_mode (* (DECAWM) *)
| Auto_repeat_keys (* (DECARM) *)
| Send_mouse_x_y_on_button_press
| Show_toolbar (* (rxvt) *)
| Start_blinking_cursor (* (att610) *)
| Print_form_feed (* (DECPFF) *)
| Set_print_extent_to_full_screen (* (DECPEX) *)
| Show_cursor (* (DECTCEM) *)
| Show_scrollbar (* (rxvt) *)
| Enable_font_shifting_functions (* (rxvt) *)
| Enter_tektronix_mode (* (DECTEK) *)
| Allow_80_to_132_mode
| More_fix (* As in `more(1)` (see curses resource)  *)
| Enable_nation_replacement_character_sets (* (DECNRCM) *)
| Turn_on_margin_bell
| Reverse_wraparound_mode
| Start_logging (* (normally disabled by a compile-time option) *)
| Use_alternate_screen_buffer (* (unless disabled by the titeInhibit resource) *)
| Application_keypad (* (DECNKM) *)
| Backarrow_key_sends_delete (* (DECBKM) *)
| Send_mouse_x_y_on_button_press_and_release
| Use_hilite_mouse_tracking
| Use_cell_motion_mouse_tracking
| Use_all_motion_mouse_tracking
| Scroll_to_bottom_on_tty_output (* (rxvt) *)
| Scroll_to_bottom_on_key_press (* (rxvt) *)
| Enable_special_modifiers_for_alt_and_numlock_keys
| Send_ESC_when_Meta_modifies_a_key (* (enables the metaSendsEscape resource) *)
| Send_DEL_from_the_editing_keypad_Delete_key
| Save_cursor_as_in_DECSC (* (unless disabled by the titeInhibit resource) *)
| Save_cursor_as_in_DECSC_and_use_alternate_screen_buffer (* clearing alt buffer first (unless disabled by the titeInhibit resource). This combines the effects of the 1 0 4 7 and 1 0 4 8 modes. Use this with terminfo-based applications rather than the 4 7 mode.  *)
| Set_Sun_function_key_mode
| Set_HP_function_key_mode
| Set_SCO_function_key_mode
| Set_legacy_keyboard_emulation (* (X11R6) *)
| Set_Sun_PC_keyboard_emulation_of_VT220_keyboard
[@@deriving sexp, compare]

let of_int = function
| 1 -> Application_cursor_keys
| 2 -> Designate_USASCII
| 3 -> N_132_column_mode
| 4 -> Smooth_slow_scroll
| 5 -> Reverse_video
| 6 -> Origin_mode
| 7 -> Wraparound_mode
| 8 -> Auto_repeat_keys
| 9 -> Send_mouse_x_y_on_button_press
| 10 -> Show_toolbar
| 12 -> Start_blinking_cursor
| 18 -> Print_form_feed
| 19 -> Set_print_extent_to_full_screen
| 25 -> Show_cursor
| 30 -> Show_scrollbar
| 35 -> Enable_font_shifting_functions
| 38 -> Enter_tektronix_mode
| 40 -> Allow_80_to_132_mode
| 41 -> More_fix
| 42 -> Enable_nation_replacement_character_sets
| 44 -> Turn_on_margin_bell
| 45 -> Reverse_wraparound_mode
| 46 -> Start_logging
| 47 -> Use_alternate_screen_buffer
| 66 -> Application_keypad
| 67 -> Backarrow_key_sends_delete
| 1000 -> Send_mouse_x_y_on_button_press_and_release
| 1001 -> Use_hilite_mouse_tracking
| 1002 -> Use_cell_motion_mouse_tracking
| 1003 -> Use_all_motion_mouse_tracking
| 1010 -> Scroll_to_bottom_on_tty_output
| 1011 -> Scroll_to_bottom_on_key_press
| 1035 -> Enable_special_modifiers_for_alt_and_numlock_keys
| 1036 -> Send_ESC_when_Meta_modifies_a_key
| 1037 -> Send_DEL_from_the_editing_keypad_Delete_key
| 1047 -> Use_alternate_screen_buffer
| 1048 -> Save_cursor_as_in_DECSC
| 1049 -> Save_cursor_as_in_DECSC_and_use_alternate_screen_buffer
| 1051 -> Set_Sun_function_key_mode
| 1052 -> Set_HP_function_key_mode
| 1053 -> Set_SCO_function_key_mode
| 1060 -> Set_legacy_keyboard_emulation
| 1061 -> Set_Sun_PC_keyboard_emulation_of_VT220_keyboard
| n -> failwithf "Unknown DEC Private Mode %d" n ()
