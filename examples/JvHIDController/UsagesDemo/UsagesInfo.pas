{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit UsagesInfo;

interface

uses
  Hid, HidUsage;

procedure UsageAndUsagePageText(UsagePage, Usage: TUsage; var UsagePageText, UsageText: string);

implementation

uses
 SysUtils;

procedure UsageAndUsagePageText(UsagePage, Usage: TUsage; var UsagePageText, UsageText: string);
begin
  UsagePageText := '';
  UsageText := '';
  case UsagePage of
    HID_USAGE_PAGE_UNDEFINED:
      UsagePageText := 'Undefined';
    HID_USAGE_PAGE_GENERIC:
      begin
        UsagePageText := 'Generic Desktop';
        case Usage of
          HID_USAGE_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_GENERIC_POINTER:
            UsageText := 'Pointing Device';
          HID_USAGE_GENERIC_MOUSE:
            UsageText := 'Mouse';
          HID_USAGE_GENERIC_RESERVED1:
            UsageText := 'Reserved';
          HID_USAGE_GENERIC_JOYSTICK:
            UsageText := 'Joystick';
          HID_USAGE_GENERIC_GAMEPAD:
            UsageText := 'Gamepad';
          HID_USAGE_GENERIC_KEYBOARD:
            UsageText := 'Keyboard';
          HID_USAGE_GENERIC_KEYPAD:
            UsageText := 'Keypad';
          HID_USAGE_GENERIC_MULTIAXIS:
            UsageText := 'Multi-Axis';
          HID_USAGE_GENERIC_X:
            UsageText := 'X Axis';
          HID_USAGE_GENERIC_Y:
            UsageText := 'Y Axis';
          HID_USAGE_GENERIC_Z:
            UsageText := 'Z Axis';
          HID_USAGE_GENERIC_RX:
            UsageText := 'Rotational X Axis';
          HID_USAGE_GENERIC_RY:
            UsageText := 'Rotational Y Axis';
          HID_USAGE_GENERIC_RZ:
            UsageText := 'Rotational Z Axis';
          HID_USAGE_GENERIC_SLIDER:
            UsageText := 'Slider';
          HID_USAGE_GENERIC_DIAL:
            UsageText := 'Dial';
          HID_USAGE_GENERIC_WHEEL:
            UsageText := 'Wheel';
          HID_USAGE_GENERIC_HATSWITCH:
            UsageText := 'Hat Switch';
          HID_USAGE_GENERIC_COUNTED_BUFFER:
            UsageText := 'Counted Buffer';
          HID_USAGE_GENERIC_BYTE_COUNT:
            UsageText := 'Byte Count';
          HID_USAGE_GENERIC_MOTION_WAKEUP:
            UsageText := 'Motion Wakeup';
          HID_USAGE_GENERIC_START:
            UsageText := 'Start';
          HID_USAGE_GENERIC_SELECT:
            UsageText := 'Select';
          HID_USAGE_GENERIC_RESERVED2:
            UsageText := 'Reserved';
          HID_USAGE_GENERIC_VX:
            UsageText := 'Velocity X';
          HID_USAGE_GENERIC_VY:
            UsageText := 'Velocity Y';
          HID_USAGE_GENERIC_VZ:
            UsageText := 'Velocity Z';
          HID_USAGE_GENERIC_VBRX:
            UsageText := 'Velocity Brake X';
          HID_USAGE_GENERIC_VBRY:
            UsageText := 'Velocity Brake Y';
          HID_USAGE_GENERIC_VBRZ:
            UsageText := 'Velocity Brake Z';
          HID_USAGE_GENERIC_VNO:
            UsageText := 'Vno';
          HID_USAGE_FEATURE_NOTIFICATION:
            UsageText := 'Feature Notification';
          HID_USAGE_GENERIC_SYSTEM_CTL:
            UsageText := 'System Control';
          HID_USAGE_GENERIC_SYSCTL_POWER:
            UsageText := 'System Control Power Down';
          HID_USAGE_GENERIC_SYSCTL_SLEEP:
            UsageText := 'System Control Sleep';
          HID_USAGE_GENERIC_SYSCTL_WAKE:
            UsageText := 'System Control Wake Up';
          HID_USAGE_GENERIC_SYSCTL_CONTEXT_MENU:
            UsageText := 'System Control Context Menu';
          HID_USAGE_GENERIC_SYSCTL_MAIN_MENU:
            UsageText := 'System Control Main Menu';
          HID_USAGE_GENERIC_SYSCTL_APP_MENU:
            UsageText := 'System Control App Menu';
          HID_USAGE_GENERIC_SYSCTL_HELP_MENU:
            UsageText := 'System Control Help Menu';
          HID_USAGE_GENERIC_SYSCTL_MENU_EXIT:
            UsageText := 'System Control Menu Exit';
          HID_USAGE_GENERIC_SYSCTL_MENU_SELECT:
            UsageText := 'System Control Menu Select';
          HID_USAGE_GENERIC_SYSCTL_MENU_RIGHT:
            UsageText := 'System Control Menu Right';
          HID_USAGE_GENERIC_SYSCTL_MENU_LEFT:
            UsageText := 'System Control Menu Left';
          HID_USAGE_GENERIC_SYSCTL_MENU_UP:
            UsageText := 'System Control Menu Up';
          HID_USAGE_GENERIC_SYSCTL_MENU_DOWN:
            UsageText := 'System Control Menu Down';
          HID_USAGE_GENERIC_SYSCTL_COLD_RESTART:
            UsageText := 'System Control Cold Restart';
          HID_USAGE_GENERIC_SYSCTL_WARM_RESTART:
            UsageText := 'System Control Warm Restart';
          HID_USAGE_GENERIC_SYSCTL_DPAD_UP:
            UsageText := 'System Control DPad Up';
          HID_USAGE_GENERIC_SYSCTL_DPAD_DOWN:
            UsageText := 'System Control DPad Down';
          HID_USAGE_GENERIC_SYSCTL_DPAD_RIGHT:
            UsageText := 'System Control DPad Right';
          HID_USAGE_GENERIC_SYSCTL_DPAD_LEFT:
            UsageText := 'System Control DPad Left';
          HID_USAGE_GENERIC_SYSCTL_DOCK:
            UsageText := 'System Control Dock';
          HID_USAGE_GENERIC_SYSCTL_UNDOCK:
            UsageText := 'System Control Undock';
          HID_USAGE_GENERIC_SYSCTL_SETUP:
            UsageText := 'System Control Setup';
          HID_USAGE_GENERIC_SYSCTL_BREAK:
            UsageText := 'System Control Break';
          HID_USAGE_GENERIC_SYSCTL_DEBUGGER_BREAK:
            UsageText := 'System Control Debugger Break';
          HID_USAGE_GENERIC_SYSCTL_APP_BREAK:
            UsageText := 'System Control Application Break';
          HID_USAGE_GENERIC_SYSCTL_APP_DEBUGGER_BREAK:
            UsageText := 'System Control Application Debugger Break';
          HID_USAGE_GENERIC_SYSCTL_SYSTEM_SPEAKER_MUTE:
            UsageText := 'System Control Speaker Mute';
          HID_USAGE_GENERIC_SYSCTL_SYSTEM_HIBERNATE:
            UsageText := 'System Control Hibernate';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_INVERT:
            UsageText := 'System Control Invert Display';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_INTERNAL:
            UsageText := 'System Control Internal Display';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_EXTERNAL:
            UsageText := 'System Control External Display';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_BOTH:
            UsageText := 'System Control Both Displays';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_DUAL:
            UsageText := 'System Control Dual Displays';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_TOGGLE_INT_EXT:
            UsageText := 'System Control Toggle Internal/External Display';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_SWAP:
            UsageText := 'System Control Swap Primary/Secondary Displays';
          HID_USAGE_GENERIC_SYSCTL_DISPLAY_LCD_AUTOSCALE:
            UsageText := 'System Control LCD Autoscale Display';
        end;
      end;
    HID_USAGE_PAGE_SIMULATION:
      begin
        UsagePageText := 'Simulation';
        case Usage of
          HID_USAGE_SIMULATION_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_SIMULATION_FLIGHT:
            UsageText := 'Flight';
          HID_USAGE_SIMULATION_AUTOMOBILE:
            UsageText := 'Automobile';
          HID_USAGE_SIMULATION_TANK:
            UsageText := 'Tank';
          HID_USAGE_SIMULATION_SPACESHIP:
            UsageText := 'Spaceship';
          HID_USAGE_SIMULATION_SUBMARINE:
            UsageText := 'Submarine';
          HID_USAGE_SIMULATION_SAILING:
            UsageText := 'Sailing';
          HID_USAGE_SIMULATION_MOTORCYCLE:
            UsageText := 'Motorcycle';
          HID_USAGE_SIMULATION_SPORTS:
            UsageText := 'Sports';
          HID_USAGE_SIMULATION_AIRPLANE:
            UsageText := 'Airplane';
          HID_USAGE_SIMULATION_HELICOPTER:
            UsageText := 'Helicopter';
          HID_USAGE_SIMULATION_MAGIC_CARPET:
            UsageText := 'Magic Carpet';
          HID_USAGE_SIMULATION_BICYCLE:
            UsageText := 'Bicycle';
          HID_USAGE_SIMULATION_FLIGHT_CONTROL_STICK:
            UsageText := 'Flight Control Stick';
          HID_USAGE_SIMULATION_FLIGHT_STICK:
            UsageText := 'Flight Stick';
          HID_USAGE_SIMULATION_CYCLIC_CONTROL:
            UsageText := 'Cyclic Control';
          HID_USAGE_SIMULATION_CYCLIC_TRIM:
            UsageText := 'Cyclic Trim';
          HID_USAGE_SIMULATION_FLIGHT_YOKE:
            UsageText := 'Flight Yoke';
          HID_USAGE_SIMULATION_TRACK_CONTROL:
            UsageText := 'Track Control';
          HID_USAGE_SIMULATION_AILERON:
            UsageText := 'Aileron';
          HID_USAGE_SIMULATION_AILERON_TRIM:
            UsageText := 'Aileron Trim';
          HID_USAGE_SIMULATION_ANTITORQUE_CONTROL:
            UsageText := 'Anti-Torque Control';
          HID_USAGE_SIMULATION_AUTOPILOT_ENABLE:
            UsageText := 'Enable Autopilot';
          HID_USAGE_SIMULATION_CHAFF_RELEASE:
            UsageText := 'Chaff Release';
          HID_USAGE_SIMULATION_COLLECTIVE_CONTROL:
            UsageText := 'Collective Control';
          HID_USAGE_SIMULATION_DIVE_BREAK:
            UsageText := 'Dive Break';
          HID_USAGE_SIMULATION_ELECTRONIC_COUNTERMEASURES:
            UsageText := 'Electronic Countermeasures';
          HID_USAGE_SIMULATION_ELEVATOR:
            UsageText := 'Elevator';
          HID_USAGE_SIMULATION_ELEVATOR_TRIM:
            UsageText := 'Elevator Trim';
          HID_USAGE_SIMULATION_RUDDER:
            UsageText := 'Rudder';
          HID_USAGE_SIMULATION_THROTTLE:
            UsageText := 'Throttle';
          HID_USAGE_SIMULATION_FLIGHT_COMMUNICATIONS:
            UsageText := 'Flight Communications';
          HID_USAGE_SIMULATION_FLARE_RELEASE:
            UsageText := 'Flare Release';
          HID_USAGE_SIMULATION_LANDING_GEAR:
            UsageText := 'Landing Gear';
          HID_USAGE_SIMULATION_TOE_BRAKE:
            UsageText := 'Toe Brake';
          HID_USAGE_SIMULATION_TRIGGER:
            UsageText := 'Trigger';
          HID_USAGE_SIMULATION_WEAPONS_ARM:
            UsageText := 'Arm Weapons';
          HID_USAGE_SIMULATION_WEAPONS_SELECT:
            UsageText := 'Select Weapons';
          HID_USAGE_SIMULATION_WING_FLAPS:
            UsageText := 'Wing Flaps';
          HID_USAGE_SIMULATION_ACCELERATOR:
            UsageText := 'Accelerator';
          HID_USAGE_SIMULATION_BRAKE:
            UsageText := 'Brake';
          HID_USAGE_SIMULATION_CLUTCH:
            UsageText := 'Clutch';
          HID_USAGE_SIMULATION_SHIFTER:
            UsageText := 'Shifter';
          HID_USAGE_SIMULATION_STEERING:
            UsageText := 'Steering';
          HID_USAGE_SIMULATION_TURRET_DIRECTION:
            UsageText := 'Turret Direction';
          HID_USAGE_SIMULATION_BARREL_ELEVATION:
            UsageText := 'Barrel Elevation';
          HID_USAGE_SIMULATION_DIVE_PLANE:
            UsageText := 'Dive Plane';
          HID_USAGE_SIMULATION_BALLAST:
            UsageText := 'Ballast';
          HID_USAGE_SIMULATION_BICYCLE_CRANK:
            UsageText := 'Bicycle Crank';
          HID_USAGE_SIMULATION_HANDLE_BARS:
            UsageText := 'Handle Bars';
          HID_USAGE_SIMULATION_FRONT_BRAKE:
            UsageText := 'Front Brake';
          HID_USAGE_SIMULATION_REAR_BRAKE:
            UsageText := 'Rear Brake';
        end;
      end;
    HID_USAGE_PAGE_VR:
      begin
        UsagePageText := 'Virtual Reality';
        case Usage of
          HID_USAGE_VR_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_VR_BELT:
            UsageText := 'Belt';
          HID_USAGE_VR_BODY_SUIT:
            UsageText := 'Body Suit';
          HID_USAGE_VR_FLEXOR:
            UsageText := 'Flexor';
          HID_USAGE_VR_GLOVE:
            UsageText := 'Glove';
          HID_USAGE_VR_HEAD_TRACKER:
            UsageText := 'Head Tracker';
          HID_USAGE_VR_HEAD_MOUNTED_DISPLAY:
            UsageText := 'Head Mounted Display';
          HID_USAGE_VR_HAND_TRACKER:
            UsageText := 'Hand Tracker';
          HID_USAGE_VR_OCULOMETER:
            UsageText := 'Oculometer';
          HID_USAGE_VR_VEST:
            UsageText := 'Vest';
          HID_USAGE_VR_ANIMATRONIC_DEVICE:
            UsageText := 'Animatronic Device';
          HID_USAGE_VR_STEREO_ENABLE:
            UsageText := 'Stereo Enable';
          HID_USAGE_VR_DISPLAY_ENABLE:
            UsageText := 'Display Enable';
        end;
      end;
    HID_USAGE_PAGE_SPORT:
      begin
        UsagePageText := 'Sport';
        case Usage of
          HID_USAGE_SPORT_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_SPORT_BASEBALL_BAT:
            UsageText := 'Baseball Bat';
          HID_USAGE_SPORT_GOLF_CLUB:
            UsageText := 'Golf Club';
          HID_USAGE_SPORT_ROWING_MACHINE:
            UsageText := 'Rowing Machine';
          HID_USAGE_SPORT_TREADMILL:
            UsageText := 'Treadmill';
          HID_USAGE_SPORT_OAR:
            UsageText := 'Oar';
          HID_USAGE_SPORT_SLOPE:
            UsageText := 'Slope';
          HID_USAGE_SPORT_RATE:
            UsageText := 'Rate';
          HID_USAGE_SPORT_STICK_SPEED:
            UsageText := 'Stick Speed';
          HID_USAGE_SPORT_STICK_FACE_ANGLE:
            UsageText := 'Stick Face Angle';
          HID_USAGE_SPORT_STICK_HEEL_TOE:
            UsageText := 'Stick Heel/Toe';
          HID_USAGE_SPORT_STICK_FOLLOW_THROUGH:
            UsageText := 'Stick Follow Through';
          HID_USAGE_SPORT_STICK_TEMPO:
            UsageText := 'Stick Tempo';
          HID_USAGE_SPORT_STICK_TYPE:
            UsageText := 'Stick Type';
          HID_USAGE_SPORT_STICK_HEIGHT:
            UsageText := 'Stick Height';
          HID_USAGE_SPORT_PUTTER:
            UsageText := 'Putter';
          HID_USAGE_SPORT_IRON_1:
            UsageText := 'Iron 1';
          HID_USAGE_SPORT_IRON_2:
            UsageText := 'Iron 2';
          HID_USAGE_SPORT_IRON_3:
            UsageText := 'Iron 3';
          HID_USAGE_SPORT_IRON_4:
            UsageText := 'Iron 4';
          HID_USAGE_SPORT_IRON_5:
            UsageText := 'Iron 5';
          HID_USAGE_SPORT_IRON_6:
            UsageText := 'Iron 6';
          HID_USAGE_SPORT_IRON_7:
            UsageText := 'Iron 7';
          HID_USAGE_SPORT_IRON_8:
            UsageText := 'Iron 8';
          HID_USAGE_SPORT_IRON_9:
            UsageText := 'Iron 9';
          HID_USAGE_SPORT_IRON_10:
            UsageText := 'Iron 10';
          HID_USAGE_SPORT_IRON_11:
            UsageText := 'Iron 11';
          HID_USAGE_SPORT_SAND_WEDGE:
            UsageText := 'Sand Wedge';
          HID_USAGE_SPORT_LOFT_WEDGE:
            UsageText := 'Loft Wedge';
          HID_USAGE_SPORT_POWER_WEDGE:
            UsageText := 'Power Wedge';
          HID_USAGE_SPORT_WOOD_1:
            UsageText := 'Wood 1';
          HID_USAGE_SPORT_WOOD_3:
            UsageText := 'Wood 3';
          HID_USAGE_SPORT_WOOD_5:
            UsageText := 'Wood 5';
          HID_USAGE_SPORT_WOOD_7:
            UsageText := 'Wood 7';
          HID_USAGE_SPORT_WOOD_9:
            UsageText := 'Wood 9';
        end;
      end;
    HID_USAGE_PAGE_GAME:
      begin
        UsagePageText := 'Game';
        case Usage of
          HID_USAGE_GAME_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_GAME_3D_GAME_CONTROLLER:
            UsageText := '3D Game Controller';
          HID_USAGE_GAME_PINBALL_DEVICE:
            UsageText := 'Pinball Device';
          HID_USAGE_GAME_GUN_DEVICE:
            UsageText := 'Gun Device';
          HID_USAGE_GAME_POINT_OF_VIEW:
            UsageText := 'Point of View';
          HID_USAGE_GAME_TURN_RIGHT_LEFT:
            UsageText := 'Turn Right/Left';
          HID_USAGE_GAME_PITCH_FORWARD_BACKWARD:
            UsageText := 'Pitch Forward/Backward';
          HID_USAGE_GAME_ROLL_RIGHT_LEFT:
            UsageText := 'Roll Right/Left';
          HID_USAGE_GAME_MOVE_RIGHT_LEFT:
            UsageText := 'Move Right/Left';
          HID_USAGE_GAME_MOVE_FORWARD_BACKWARD:
            UsageText := 'Move Forward/Backward';
          HID_USAGE_GAME_MOVE_UP_DOWN:
            UsageText := 'Move Up/Down';
          HID_USAGE_GAME_LEAN_RIGHT_LEFT:
            UsageText := 'Lean Right/Left';
          HID_USAGE_GAME_LEAN_FORWARD_BACKWARD:
            UsageText := 'Lean Forward/Backward';
          HID_USAGE_GAME_HEIGHT_OF_POV:
            UsageText := 'Height of POV';
          HID_USAGE_GAME_FLIPPER:
            UsageText := 'Flipper';
          HID_USAGE_GAME_SECONDARY_FLIPPER:
            UsageText := 'Secondary Flipper';
          HID_USAGE_GAME_BUMP:
            UsageText := 'Bump';
          HID_USAGE_GAME_NEW_GAME:
            UsageText := 'New Game';
          HID_USAGE_GAME_SHOOT_BALL:
            UsageText := 'Shoot Ball';
          HID_USAGE_GAME_PLAYER:
            UsageText := 'Player';
          HID_USAGE_GAME_GUN_BOLT:
            UsageText := 'Gun Bolt';
          HID_USAGE_GAME_GUN_CLIP:
            UsageText := 'Gun Clip';
          HID_USAGE_GAME_GUN_SELECTOR:
            UsageText := 'Gun Selector';
          HID_USAGE_GAME_GUN_SINGLE_SHOT:
            UsageText := 'Gun Single Shot';
          HID_USAGE_GAME_GUN_BURST:
            UsageText := 'Gun Burst';
          HID_USAGE_GAME_GUN_AUTOMATIC:
            UsageText := 'Gun Automatic';
          HID_USAGE_GAME_GUN_SAFETY:
            UsageText := 'Gun Safety';
          HID_USAGE_GAME_GAMEPAD_FIRE_JUMP:
            UsageText := 'Gamepad Fire/Jump';
          HID_USAGE_GAME_GAMEPAD_TRIGGER:
            UsageText := 'Gamepad Trigger';
        end;
      end;
    HID_USAGE_PAGE_GENERIC_GAME_CONTROLS:
      begin
        UsagePageText := 'Generic';
        case Usage of
          HID_USAGE_GENERIC_GAME_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_GENERIC_GAME_BATTERY_STRENGTH:
            UsageText := 'Battery Strength';
          HID_USAGE_GENERIC_GAME_WIRELESS_CHANNEL:
            UsageText := 'Wireless Channel';
          HID_USAGE_GENERIC_GAME_WIRELESS_ID:
            UsageText := 'Wireless ID';
        end;
      end;
    HID_USAGE_PAGE_KEYBOARD:
      begin
        UsagePageText := 'Keyboard';
        case Usage of
          HID_USAGE_KEYBOARD_NOEVENT:
            UsageText := 'Empty';
          HID_USAGE_KEYBOARD_ROLLOVER:
            UsageText := 'Rollover';
          HID_USAGE_KEYBOARD_POSTFAIL:
            UsageText := 'Postfail';
          HID_USAGE_KEYBOARD_UNDEFINED:
            UsageText := 'Undefined Error';
          HID_USAGE_KEYBOARD_aA:
            UsageText := 'A';
          HID_USAGE_KEYBOARD_bB:
            UsageText := 'B';
          HID_USAGE_KEYBOARD_cC:
            UsageText := 'C';
          HID_USAGE_KEYBOARD_dD:
            UsageText := 'D';
          HID_USAGE_KEYBOARD_eE:
            UsageText := 'E';
          HID_USAGE_KEYBOARD_fF:
            UsageText := 'F';
          HID_USAGE_KEYBOARD_gG:
            UsageText := 'G';
          HID_USAGE_KEYBOARD_hH:
            UsageText := 'H';
          HID_USAGE_KEYBOARD_iI:
            UsageText := 'I';
          HID_USAGE_KEYBOARD_jJ:
            UsageText := 'J';
          HID_USAGE_KEYBOARD_kK:
            UsageText := 'K';
          HID_USAGE_KEYBOARD_lL:
            UsageText := 'L';
          HID_USAGE_KEYBOARD_mM:
            UsageText := 'M';
          HID_USAGE_KEYBOARD_nN:
            UsageText := 'N';
          HID_USAGE_KEYBOARD_oO:
            UsageText := 'O';
          HID_USAGE_KEYBOARD_pP:
            UsageText := 'P';
          HID_USAGE_KEYBOARD_qQ:
            UsageText := 'Q';
          HID_USAGE_KEYBOARD_rR:
            UsageText := 'R';
          HID_USAGE_KEYBOARD_sS:
            UsageText := 'S';
          HID_USAGE_KEYBOARD_tT:
            UsageText := 'T';
          HID_USAGE_KEYBOARD_uU:
            UsageText := 'U';
          HID_USAGE_KEYBOARD_vV:
            UsageText := 'V';
          HID_USAGE_KEYBOARD_wW:
            UsageText := 'W';
          HID_USAGE_KEYBOARD_xX:
            UsageText := 'X';
          HID_USAGE_KEYBOARD_yY:
            UsageText := 'Y';
          HID_USAGE_KEYBOARD_zZ:
            UsageText := 'Z';
          HID_USAGE_KEYBOARD_ONE:
            UsageText := '1';
          HID_USAGE_KEYBOARD_TWO:
            UsageText := '2';
          HID_USAGE_KEYBOARD_THREE:
            UsageText := '3';
          HID_USAGE_KEYBOARD_FOUR:
            UsageText := '4';
          HID_USAGE_KEYBOARD_FIVE:
            UsageText := '5';
          HID_USAGE_KEYBOARD_SIX:
            UsageText := '6';
          HID_USAGE_KEYBOARD_SEVEN:
            UsageText := '7';
          HID_USAGE_KEYBOARD_EIGHT:
            UsageText := '8';
          HID_USAGE_KEYBOARD_NINE:
            UsageText := '9';
          HID_USAGE_KEYBOARD_ZERO:
            UsageText := '0';
          HID_USAGE_KEYBOARD_ENTER:
            UsageText := 'Enter (not Return)';
          HID_USAGE_KEYBOARD_ESCAPE:
            UsageText := 'Esc';
          HID_USAGE_KEYBOARD_BACKSPACE:
            UsageText := 'Backspace';
          HID_USAGE_KEYBOARD_TAB:
            UsageText := 'Tabulator';
          HID_USAGE_KEYBOARD_SPACE:
            UsageText := 'Space';
          HID_USAGE_KEYBOARD_MINUS:
            UsageText := '-';
          HID_USAGE_KEYBOARD_EQUAL:
            UsageText := '=';
          HID_USAGE_KEYBOARD_LSQBRACKET:
            UsageText := '[';
          HID_USAGE_KEYBOARD_RSQBRACKET:
            UsageText := ']';
          HID_USAGE_KEYBOARD_BACKSLASH:
            UsageText := '\';
          HID_USAGE_KEYBOARD_HASHMARK2:
            UsageText := '# 2';
          HID_USAGE_KEYBOARD_SEMICOLON:
            UsageText := ';';
          HID_USAGE_KEYBOARD_APOSTROPH:
            UsageText := '''';
          HID_USAGE_KEYBOARD_GRAVEACCENT:
            UsageText := '`';
          HID_USAGE_KEYBOARD_COMMA:
            UsageText := ',';
          HID_USAGE_KEYBOARD_DOT:
            UsageText := '.';
          HID_USAGE_KEYBOARD_SLASH:
            UsageText := '/';
          HID_USAGE_KEYBOARD_CAPS_LOCK:
            UsageText := 'Caps Lock';
          HID_USAGE_KEYBOARD_F1:
            UsageText := 'F1';
          HID_USAGE_KEYBOARD_F2:
            UsageText := 'F2';
          HID_USAGE_KEYBOARD_F3:
            UsageText := 'F3';
          HID_USAGE_KEYBOARD_F4:
            UsageText := 'F4';
          HID_USAGE_KEYBOARD_F5:
            UsageText := 'F5';
          HID_USAGE_KEYBOARD_F6:
            UsageText := 'F6';
          HID_USAGE_KEYBOARD_F7:
            UsageText := 'F7';
          HID_USAGE_KEYBOARD_F8:
            UsageText := 'F8';
          HID_USAGE_KEYBOARD_F9:
            UsageText := 'F9';
          HID_USAGE_KEYBOARD_F10:
            UsageText := 'F10';
          HID_USAGE_KEYBOARD_F11:
            UsageText := 'F11';
          HID_USAGE_KEYBOARD_F12:
            UsageText := 'F12';
          HID_USAGE_KEYBOARD_PRINT_SCREEN:
            UsageText := 'Print Screen';
          HID_USAGE_KEYBOARD_SCROLL_LOCK:
            UsageText := 'Scroll Lock';
          HID_USAGE_KEYBOARD_PAUSE:
            UsageText := 'Pause';
          HID_USAGE_KEYBOARD_INSERT:
            UsageText := 'Ins';
          HID_USAGE_KEYBOARD_HOME:
            UsageText := 'Home';
          HID_USAGE_KEYBOARD_PAGEUP:
            UsageText := 'PgUp';
          HID_USAGE_KEYBOARD_DELETE:
            UsageText := 'Del';
          HID_USAGE_KEYBOARD_END:
            UsageText := 'End';
          HID_USAGE_KEYBOARD_PAGEDOWN:
            UsageText := 'PgDn';
          HID_USAGE_KEYBOARD_RIGHT:
            UsageText := 'Right Arrow';
          HID_USAGE_KEYBOARD_LEFT:
            UsageText := 'Left Arrow';
          HID_USAGE_KEYBOARD_DOWN:
            UsageText := 'Down Arrow';
          HID_USAGE_KEYBOARD_UP:
            UsageText := 'Up Arrow';
          HID_USAGE_KEYPAD_NUM_LOCK:
            UsageText := 'Keypad Num Lock';
          HID_USAGE_KEYPAD_SLASH:
            UsageText := 'Keypad /';
          HID_USAGE_KEYPAD_STAR:
            UsageText := 'Keypad *';
          HID_USAGE_KEYPAD_MINUS:
            UsageText := 'Keypad -';
          HID_USAGE_KEYPAD_PLUS:
            UsageText := 'Keypad +';
          HID_USAGE_KEYPAD_ENTER:
            UsageText := 'Keypad Enter';
          HID_USAGE_KEYPAD_ONE:
            UsageText := 'Keypad 1';
          HID_USAGE_KEYPAD_TWO:
            UsageText := 'Keypad 2';
          HID_USAGE_KEYPAD_THREE:
            UsageText := 'Keypad 3';
          HID_USAGE_KEYPAD_FOUR:
            UsageText := 'Keypad 4';
          HID_USAGE_KEYPAD_FIVE:
            UsageText := 'Keypad 5';
          HID_USAGE_KEYPAD_SIX:
            UsageText := 'Keypad 6';
          HID_USAGE_KEYPAD_SEVEN:
            UsageText := 'Keypad 7';
          HID_USAGE_KEYPAD_EIGHT:
            UsageText := 'Keypad 8';
          HID_USAGE_KEYPAD_NINE:
            UsageText := 'Keypad 9';
          HID_USAGE_KEYPAD_ZERO:
            UsageText := 'Keypad 0';
          HID_USAGE_KEYPAD_DOT:
            UsageText := 'Keypad .';
          HID_USAGE_KEYBOARD_BACKSLASH2:
            UsageText := '\ 2';
          HID_USAGE_KEYBOARD_APPLICATION:
            UsageText := 'Application';
          HID_USAGE_KEYBOARD_POWER:
            UsageText := 'Power';
          HID_USAGE_KEYPAD_EQUAL2:
            UsageText := 'Keypad = 2';
          HID_USAGE_KEYBOARD_F13:
            UsageText := 'F13';
          HID_USAGE_KEYBOARD_F14:
            UsageText := 'F14';
          HID_USAGE_KEYBOARD_F15:
            UsageText := 'F15';
          HID_USAGE_KEYBOARD_F16:
            UsageText := 'F16';
          HID_USAGE_KEYBOARD_F17:
            UsageText := 'F17';
          HID_USAGE_KEYBOARD_F18:
            UsageText := 'F18';
          HID_USAGE_KEYBOARD_F19:
            UsageText := 'F19';
          HID_USAGE_KEYBOARD_F20:
            UsageText := 'F20';
          HID_USAGE_KEYBOARD_F21:
            UsageText := 'F21';
          HID_USAGE_KEYBOARD_F22:
            UsageText := 'F22';
          HID_USAGE_KEYBOARD_F23:
            UsageText := 'F23';
          HID_USAGE_KEYBOARD_F24:
            UsageText := 'F24';
          HID_USAGE_KEYBOARD_EXECUTE:
            UsageText := 'Execute';
          HID_USAGE_KEYBOARD_HELP:
            UsageText := 'Help';
          HID_USAGE_KEYBOARD_MENU:
            UsageText := 'Menu';
          HID_USAGE_KEYBOARD_SELECT:
            UsageText := 'Select';
          HID_USAGE_KEYBOARD_STOP:
            UsageText := 'Stop';
          HID_USAGE_KEYBOARD_AGAIN:
            UsageText := 'Again';
          HID_USAGE_KEYBOARD_UNDO:
            UsageText := 'Undo';
          HID_USAGE_KEYBOARD_CUT:
            UsageText := 'Cut';
          HID_USAGE_KEYBOARD_COPY:
            UsageText := 'Copy';
          HID_USAGE_KEYBOARD_PASTE:
            UsageText := 'Paste';
          HID_USAGE_KEYBOARD_FIND:
            UsageText := 'Find';
          HID_USAGE_KEYBOARD_MUTE:
            UsageText := 'Mute';
          HID_USAGE_KEYBOARD_VOLUME_UP:
            UsageText := 'Volume Up';
          HID_USAGE_KEYBOARD_VOLUME_DOWN:
            UsageText := 'Volume Down';
          HID_USAGE_KEYBOARD_LOCKCAPS:
            UsageText := 'Caps Lock';
          HID_USAGE_KEYBOARD_LOCKNUM:
            UsageText := 'Num Lock';
          HID_USAGE_KEYBOARD_LOCKSCROLL:
            UsageText := 'Scroll Lock';
          HID_USAGE_KEYPAD_COMMA:
            UsageText := 'Keypad ,';
          HID_USAGE_KEYPAD_EQUALSIGN:
            UsageText := 'Keypad =';
          HID_USAGE_KEYBOARD_INATL1:
            UsageText := 'International 1';
          HID_USAGE_KEYBOARD_INATL2:
            UsageText := 'International 2';
          HID_USAGE_KEYBOARD_INATL3:
            UsageText := 'International 3';
          HID_USAGE_KEYBOARD_INATL4:
            UsageText := 'International 4';
          HID_USAGE_KEYBOARD_INATL5:
            UsageText := 'International 5';
          HID_USAGE_KEYBOARD_INATL6:
            UsageText := 'International 6';
          HID_USAGE_KEYBOARD_INATL7:
            UsageText := 'International 7';
          HID_USAGE_KEYBOARD_INATL8:
            UsageText := 'International 8';
          HID_USAGE_KEYBOARD_INATL9:
            UsageText := 'International 9';
          HID_USAGE_KEYBOARD_LANG1:
            UsageText := 'Language 1';
          HID_USAGE_KEYBOARD_LANG2:
            UsageText := 'Language 2';
          HID_USAGE_KEYBOARD_LANG3:
            UsageText := 'Language 3';
          HID_USAGE_KEYBOARD_LANG4:
            UsageText := 'Language 4';
          HID_USAGE_KEYBOARD_LANG5:
            UsageText := 'Language 5';
          HID_USAGE_KEYBOARD_LANG6:
            UsageText := 'Language 6';
          HID_USAGE_KEYBOARD_LANG7:
            UsageText := 'Language 7';
          HID_USAGE_KEYBOARD_LANG8:
            UsageText := 'Language 8';
          HID_USAGE_KEYBOARD_LANG9:
            UsageText := 'Language 9';
          HID_USAGE_KEYBOARD_ALTERASE:
            UsageText := 'Alternate Erase';
          HID_USAGE_KEYBOARD_SYSREQ:
            UsageText := 'Sys Request';
          HID_USAGE_KEYBOARD_CANCEL:
            UsageText := 'Cancel';
          HID_USAGE_KEYBOARD_CLEAR:
            UsageText := 'Clear';
          HID_USAGE_KEYBOARD_PRIOR:
            UsageText := 'Prior';
          HID_USAGE_KEYBOARD_RETURN:
            UsageText := 'Return (not Enter)';
          HID_USAGE_KEYBOARD_SEPARATOR:
            UsageText := 'Separator';
          HID_USAGE_KEYBOARD_OUT:
            UsageText := 'Out';
          HID_USAGE_KEYBOARD_OPER:
            UsageText := 'Oper';
          HID_USAGE_KEYBOARD_CLEAR_AGAIN:
            UsageText := 'Clear/Again';
          HID_USAGE_KEYBOARD_CRSEL:
            UsageText := 'CrSel/Props';
          HID_USAGE_KEYBOARD_EXSEL:
            UsageText := 'ExSel';
          HID_USAGE_KEYPAD_HUNDREDS:
            UsageText := 'Keypad 00';
          HID_USAGE_KEYPAD_THOUSANDS:
            UsageText := 'Keypad 000';
          HID_USAGE_KEYPAD_THOUSANDS_SEP:
            UsageText := 'Keypad Thousands Separator';
          HID_USAGE_KEYPAD_DECIMAL_SEP:
            UsageText := 'Keypad Decimal Separator';
          HID_USAGE_KEYPAD_CURR_UNIT:
            UsageText := 'Keypad Currency Unit';
          HID_USAGE_KEYPAD_CURR_SUBUNIT:
            UsageText := 'Keypad Currency Subunit';
          HID_USAGE_KEYPAD_LROUNDBRACKET:
            UsageText := 'Keypad (';
          HID_USAGE_KEYPAD_RROUNDBRACKET:
            UsageText := 'Keypad )';
          HID_USAGE_KEYPAD_LCURLYBRACKET:
            UsageText := 'Keypad {';
          HID_USAGE_KEYPAD_RCURLYBRACKET:
            UsageText := 'Keypad }';
          HID_USAGE_KEYPAD_TABULATOR:
            UsageText := 'Keypad Tabulator';
          HID_USAGE_KEYPAD_BACKSPACE:
            UsageText := 'Keypad Backspace';
          HID_USAGE_KEYPAD_A:
            UsageText := 'Keypad A';
          HID_USAGE_KEYPAD_B:
            UsageText := 'Keypad B';
          HID_USAGE_KEYPAD_C:
            UsageText := 'Keypad C';
          HID_USAGE_KEYPAD_D:
            UsageText := 'Keypad D';
          HID_USAGE_KEYPAD_E:
            UsageText := 'Keypad E';
          HID_USAGE_KEYPAD_F:
            UsageText := 'Keypad F';
          HID_USAGE_KEYPAD_XOR:
            UsageText := 'Keypad XOR';
          HID_USAGE_KEYPAD_CIRCUMFLEX:
            UsageText := 'Keypad ^';
          HID_USAGE_KEYPAD_PERCENT:
            UsageText := 'Keypad %';
          HID_USAGE_KEYPAD_BIGGER_THAN:
            UsageText := 'Keypad <';
          HID_USAGE_KEYPAD_LESS_THAN:
            UsageText := 'Keypad >';
          HID_USAGE_KEYPAD_BINARY_AND:
            UsageText := 'Keypad &';
          HID_USAGE_KEYPAD_LOGICAL_AND:
            UsageText := 'Keypad &&';
          HID_USAGE_KEYPAD_BINARY_OR:
            UsageText := 'Keypad |';
          HID_USAGE_KEYPAD_LOGICAL_OR:
            UsageText := 'Keypad ||';
          HID_USAGE_KEYPAD_COLON:
            UsageText := 'Keypad :';
          HID_USAGE_KEYPAD_HASHMARK:
            UsageText := 'Keypad #';
          HID_USAGE_KEYPAD_SPACE:
            UsageText := 'Keypad Space';
          HID_USAGE_KEYPAD_AT:
            UsageText := 'Keypad @';
          HID_USAGE_KEYPAD_EXCLAMATION:
            UsageText := 'Keypad !';
          HID_USAGE_KEYPAD_MEM_STORE:
            UsageText := 'Keypad Memory Store';
          HID_USAGE_KEYPAD_MEM_RECALL:
            UsageText := 'Keypad Memory Recall';
          HID_USAGE_KEYPAD_MEM_CLEAR:
            UsageText := 'Keypad Memory Clear';
          HID_USAGE_KEYPAD_MEM_ADD:
            UsageText := 'Keypad Memory Add';
          HID_USAGE_KEYPAD_MEM_SUBTRACT:
            UsageText := 'Keypad Memory Subtract';
          HID_USAGE_KEYPAD_MEM_MULTIPLY:
            UsageText := 'Keypad Memory Multiply';
          HID_USAGE_KEYPAD_MEM_DIVIDE:
            UsageText := 'Keypad Memory Divide';
          HID_USAGE_KEYPAD_PLUS_MINUS:
            UsageText := 'Keypad Memory Minus';
          HID_USAGE_KEYPAD_CLEAR:
            UsageText := 'Keypad Clear';
          HID_USAGE_KEYPAD_CLEAR_ENTRY:
            UsageText := 'Keypad Clear Entry';
          HID_USAGE_KEYPAD_BINARY:
            UsageText := 'Keypad Binary';
          HID_USAGE_KEYPAD_OCTAL:
            UsageText := 'Keypad Octal';
          HID_USAGE_KEYPAD_DECIMAL:
            UsageText := 'Keypad Decimal';
          HID_USAGE_KEYPAD_HEXADECIMAL:
            UsageText := 'Keypad Hexadecimal';
          HID_USAGE_KEYPAD_RESERVED1:
            UsageText := 'Keypad Reserved1';
          HID_USAGE_KEYPAD_RESERVED2:
            UsageText := 'Keypad Reserved2';
          HID_USAGE_KEYBOARD_LCTRL:
            UsageText := 'Left Ctrl';
          HID_USAGE_KEYBOARD_LSHFT:
            UsageText := 'Left Shift';
          HID_USAGE_KEYBOARD_LALT:
            UsageText := 'Left Alt';
          HID_USAGE_KEYBOARD_LGUI:
            UsageText := 'Left GUI';
          HID_USAGE_KEYBOARD_RCTRL:
            UsageText := 'Right Ctrl';
          HID_USAGE_KEYBOARD_RSHFT:
            UsageText := 'Right Shift';
          HID_USAGE_KEYBOARD_RALT:
            UsageText := 'Right Alt';
          HID_USAGE_KEYBOARD_RGUI:
            UsageText := 'Right GUI';
        end;
      end;
    HID_USAGE_PAGE_LED:
      begin
        UsagePageText := 'LED';
        case Usage of
          HID_USAGE_LED_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_LED_NUM_LOCK:
            UsageText := 'Keyboard Num Lock';
          HID_USAGE_LED_CAPS_LOCK:
            UsageText := 'Keyboard Caps Lock';
          HID_USAGE_LED_SCROLL_LOCK:
            UsageText := 'Keyboard Scroll Lock';
          HID_USAGE_LED_COMPOSE:
            UsageText := 'Keyboard Compose';
          HID_USAGE_LED_KANA:
            UsageText := 'Keyboard Kana';
          HID_USAGE_LED_POWER:
            UsageText := 'Keyboard Power';
          HID_USAGE_LED_SHIFT:
            UsageText := 'Keyboard Shift';
          HID_USAGE_LED_DO_NOT_DISTURB:
            UsageText := 'Telephony Do Not Disturb';
          HID_USAGE_LED_MUTE:
            UsageText := 'Consumer Indicator Mute';
          HID_USAGE_LED_TONE_ENABLE:
            UsageText := 'Consumer Indicator Tone Enable';
          HID_USAGE_LED_HIGH_CUT_FILTER:
            UsageText := 'Consumer Indicator High Cut Filter';
          HID_USAGE_LED_LOW_CUT_FILTER:
            UsageText := 'Consumer Indicator Low Cut Filter';
          HID_USAGE_LED_EQUALIZER_ENABLE:
            UsageText := 'Consumer Indicator Equalizer Enable';
          HID_USAGE_LED_SOUND_FIELD_ON:
            UsageText := 'Consumer Indicator Sound Field On';
          HID_USAGE_LED_SURROUND_FIELD_ON:
            UsageText := 'Consumer Indicator Surround Field On';
          HID_USAGE_LED_REPEAT:
            UsageText := 'Consumer Indicator Repeat';
          HID_USAGE_LED_STEREO:
            UsageText := 'Consumer Indicator Stereo';
          HID_USAGE_LED_SAMPLING_RATE_DETECT:
            UsageText := 'Consumer Indicator Sampling Rate Detect';
          HID_USAGE_LED_SPINNING:
            UsageText := 'Media Transport Indicator Spinning';
          HID_USAGE_LED_CAV:
            UsageText := 'Consumer Indicator CAV';
          HID_USAGE_LED_CLV:
            UsageText := 'Consumer Indicator CLV';
          HID_USAGE_LED_RECORDING_FORMAT_DET:
            UsageText := 'Media Transport Indicator Recording Format Detect';
          HID_USAGE_LED_OFF_HOOK:
            UsageText := 'Telephony Off Hook';
          HID_USAGE_LED_RING:
            UsageText := 'Telephony Ring';
          HID_USAGE_LED_MESSAGE_WAITING:
            UsageText := 'Telephony Message Waiting';
          HID_USAGE_LED_DATA_MODE:
            UsageText := 'Telephony Data Mode';
          HID_USAGE_LED_BATTERY_OPERATION:
            UsageText := 'Battery Operation';
          HID_USAGE_LED_BATTERY_OK:
            UsageText := 'Battery Ok';
          HID_USAGE_LED_BATTERY_LOW:
            UsageText := 'Battery Low';
          HID_USAGE_LED_SPEAKER:
            UsageText := 'Telephony Speaker';
          HID_USAGE_LED_HEAD_SET:
            UsageText := 'Telephony Head Set';
          HID_USAGE_LED_HOLD:
            UsageText := 'Telephony Hold';
          HID_USAGE_LED_MICROPHONE:
            UsageText := 'Telephony Microphone';
          HID_USAGE_LED_COVERAGE:
            UsageText := 'Telephony Coverage';
          HID_USAGE_LED_NIGHT_MODE:
            UsageText := 'Telephony Night Mode';
          HID_USAGE_LED_SEND_CALLS:
            UsageText := 'Telephony Send Calls';
          HID_USAGE_LED_CALL_PICKUP:
            UsageText := 'Telephony Call Pickup';
          HID_USAGE_LED_CONFERENCE:
            UsageText := 'Telephony Conference';
          HID_USAGE_LED_STAND_BY:
            UsageText := 'Stand-by';
          HID_USAGE_LED_CAMERA_ON:
            UsageText := 'Consumer Indicator Camera On';
          HID_USAGE_LED_CAMERA_OFF:
            UsageText := 'Consumer Indicator Camera Off';
          HID_USAGE_LED_ON_LINE:
            UsageText := 'On Line';
          HID_USAGE_LED_OFF_LINE:
            UsageText := 'Off Line';
          HID_USAGE_LED_BUSY:
            UsageText := 'Busy';
          HID_USAGE_LED_READY:
            UsageText := 'Ready';
          HID_USAGE_LED_PAPER_OUT:
            UsageText := 'Printer Indicator Paper Out';
          HID_USAGE_LED_PAPER_JAM:
            UsageText := 'Printer Indicator Paper Jam';
          HID_USAGE_LED_REMOTE:
            UsageText := 'Remote';
          HID_USAGE_LED_FORWARD:
            UsageText := 'Media Transport Forward';
          HID_USAGE_LED_REVERSE:
            UsageText := 'Media Transport Reverse';
          HID_USAGE_LED_STOP:
            UsageText := 'Media Transport Stop';
          HID_USAGE_LED_REWIND:
            UsageText := 'Media Transport Rewind';
          HID_USAGE_LED_FAST_FORWARD:
            UsageText := 'Media Transport Fast Forward';
          HID_USAGE_LED_PLAY:
            UsageText := 'Media Transport Play';
          HID_USAGE_LED_PAUSE:
            UsageText := 'Media Transport Pause';
          HID_USAGE_LED_RECORD:
            UsageText := 'Media Transport Record';
          HID_USAGE_LED_ERROR:
            UsageText := 'Error';
          HID_USAGE_LED_SELECTED_INDICATOR:
            UsageText := 'Selected Indicator';
          HID_USAGE_LED_IN_USE_INDICATOR:
            UsageText := 'In Use Indicator';
          HID_USAGE_LED_MULTI_MODE_INDICATOR:
            UsageText := 'Multi Mode Indicator';
          HID_USAGE_LED_INDICATOR_ON:
            UsageText := 'Indicator On';
          HID_USAGE_LED_INDICATOR_FLASH:
            UsageText := 'Indicator Flash';
          HID_USAGE_LED_INDICATOR_SLOW_BLINK:
            UsageText := 'Indicator Slow Blink';
          HID_USAGE_LED_INDICATOR_FAST_BLINK:
            UsageText := 'Indicator Fast Blink';
          HID_USAGE_LED_INDICATOR_OFF:
            UsageText := 'Indicator Off';
          HID_USAGE_LED_FLASH_ON_TIME:
            UsageText := 'Flash On-Time';
          HID_USAGE_LED_SLOW_BLINK_ON_TIME:
            UsageText := 'Slow Blink On-Time';
          HID_USAGE_LED_SLOW_BLINK_OFF_TIME:
            UsageText := 'Slow Blink Off-Time';
          HID_USAGE_LED_FAST_BLINK_ON_TIME:
            UsageText := 'Fast Blink On-Time';
          HID_USAGE_LED_FAST_BLINK_OFF_TIME:
            UsageText := 'Fast Blink Off-Time';
          HID_USAGE_LED_INDICATOR_COLOR:
            UsageText := 'Indicator Color';
          HID_USAGE_LED_RED:
            UsageText := 'Red';
          HID_USAGE_LED_GREEN:
            UsageText := 'Green';
          HID_USAGE_LED_AMBER:
            UsageText := 'Amber';
          HID_USAGE_LED_GENERIC_INDICATOR:
            UsageText := 'Generic Indicator';
          HID_USAGE_LED_SYSTEM_SUSPEND:
            UsageText := 'System Suspend';
          HID_USAGE_LED_EXTERNAL_POWER:
            UsageText := 'External Power Connected';
        end;
      end;
    HID_USAGE_PAGE_BUTTON:
      begin
        UsagePageText := 'Button';
        case Usage of
          HID_USAGE_BUTTON_NO_BUTTON:
            UsageText := 'No Button Pressed';
            // Usage 1..65535 is the button number
        end;
      end;
    HID_USAGE_PAGE_ORDINAL:
      begin
        UsagePageText := 'Ordinal';
        case Usage of
          HID_USAGE_ORDINAL_RESERVED:
            UsageText := 'Reserved';
            // Usage 1..65535 is the ordinal number
        end;
      end;
    HID_USAGE_PAGE_TELEPHONY:
      begin
        UsagePageText := 'Telephony';
        case Usage of
          HID_USAGE_TELEPHONY_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_TELEPHONY_PHONE:
            UsageText := 'Phone';
          HID_USAGE_TELEPHONY_ANSWERING_MACHINE:
            UsageText := 'Answering Machine';
          HID_USAGE_TELEPHONY_MESSAGE_CONTROLS:
            UsageText := 'Message Controls';
          HID_USAGE_TELEPHONY_HANDSET:
            UsageText := 'Handset';
          HID_USAGE_TELEPHONY_HEADSET:
            UsageText := 'Headset';
          HID_USAGE_TELEPHONY_KEYPAD:
            UsageText := 'Keypad';
          HID_USAGE_TELEPHONY_PROGRAMMABLE_BUTTON:
            UsageText := 'Programmable Button';
          HID_USAGE_TELEPHONY_HOOK_SWITCH:
            UsageText := 'Hook Switch';
          HID_USAGE_TELEPHONY_FLASH:
            UsageText := 'Flash';
          HID_USAGE_TELEPHONY_FEATURE:
            UsageText := 'Feature';
          HID_USAGE_TELEPHONY_HOLD:
            UsageText := 'Hold';
          HID_USAGE_TELEPHONY_REDIAL:
            UsageText := 'Redial';
          HID_USAGE_TELEPHONY_TRANSFER:
            UsageText := 'Transfer';
          HID_USAGE_TELEPHONY_DROP:
            UsageText := 'Drop';
          HID_USAGE_TELEPHONY_PARK:
            UsageText := 'Park';
          HID_USAGE_TELEPHONY_FORWARD_CALLS:
            UsageText := 'Forward Calls';
          HID_USAGE_TELEPHONY_ALTERNATE_FUNCTION:
            UsageText := 'Alternate Function';
          HID_USAGE_TELEPHONY_LINE:
            UsageText := 'Line';
          HID_USAGE_TELEPHONY_SPEAKER_PHONE:
            UsageText := 'Speaker Phone';
          HID_USAGE_TELEPHONY_CONFERENCE:
            UsageText := 'Conference';
          HID_USAGE_TELEPHONY_RING_ENABLE:
            UsageText := 'Ring Enable';
          HID_USAGE_TELEPHONY_RING_SELECT:
            UsageText := 'Ring Select';
          HID_USAGE_TELEPHONY_PHONE_MUTE:
            UsageText := 'Phone Mute';
          HID_USAGE_TELEPHONY_CALLER_ID:
            UsageText := 'Caller ID';
          HID_USAGE_TELEPHONY_SEND:
            UsageText := 'Send';
          HID_USAGE_TELEPHONY_SPEED_DIAL:
            UsageText := 'Speed Dial';
          HID_USAGE_TELEPHONY_STORE_NUMBER:
            UsageText := 'Store Number';
          HID_USAGE_TELEPHONY_RECALL_NUMBER:
            UsageText := 'Recall Number';
          HID_USAGE_TELEPHONY_PHONE_DIRECTORY:
            UsageText := 'Phone Directory';
          HID_USAGE_TELEPHONY_VOICE_MAIL:
            UsageText := 'Voice Mail';
          HID_USAGE_TELEPHONY_SCREEN_CALLS:
            UsageText := 'Screen Calls';
          HID_USAGE_TELEPHONY_DO_NOT_DISTURB:
            UsageText := 'Do Not Disturb';
          HID_USAGE_TELEPHONY_MESSAGE:
            UsageText := 'Message';
          HID_USAGE_TELEPHONY_ANSWER_ON_OFF:
            UsageText := 'Answer On/Off';
          HID_USAGE_TELEPHONY_INSIDE_DIAL_TONE:
            UsageText := 'Inside Dial Tone';
          HID_USAGE_TELEPHONY_OUTSIDE_DIAL_TONE:
            UsageText := 'Outside Dial Tone';
          HID_USAGE_TELEPHONY_INSIDE_RING_TONE:
            UsageText := 'Inside Ring Tone';
          HID_USAGE_TELEPHONY_OUTSIDE_RING_TONE:
            UsageText := 'Outside Ring Tone';
          HID_USAGE_TELEPHONY_PRIORITY_RING_TONE:
            UsageText := 'Priority Ring Tone';
          HID_USAGE_TELEPHONY_INSIDE_RINGBACK:
            UsageText := 'Inside Ringback';
          HID_USAGE_TELEPHONY_PRIORITY_RINGBACK:
            UsageText := 'Priority Ringback';
          HID_USAGE_TELEPHONY_LINE_BUSY_TONE:
            UsageText := 'Line Busy Tone';
          HID_USAGE_TELEPHONY_REORDER_TONE:
            UsageText := 'Reorder Tone';
          HID_USAGE_TELEPHONY_CALL_WAITING_TONE:
            UsageText := 'Call Waiting Tone';
          HID_USAGE_TELEPHONY_CONFIRMATION_TONE_1:
            UsageText := 'Confirmation Tone 1';
          HID_USAGE_TELEPHONY_CONFIRMATION_TONE_2:
            UsageText := 'Confirmation Tone 2';
          HID_USAGE_TELEPHONY_TONES_OFF:
            UsageText := 'Tones Off';
          HID_USAGE_TELEPHONY_OUTSIDE_RINGBACK:
            UsageText := 'Outside Ringback';
          HID_USAGE_TELEPHONY_RINGER:
            UsageText := 'Ringer';
          HID_USAGE_TELEPHONY_KEY_0:
            UsageText := 'Key 0';
          HID_USAGE_TELEPHONY_KEY_1:
            UsageText := 'Key 1';
          HID_USAGE_TELEPHONY_KEY_2:
            UsageText := 'Key 2';
          HID_USAGE_TELEPHONY_KEY_3:
            UsageText := 'Key 3';
          HID_USAGE_TELEPHONY_KEY_4:
            UsageText := 'Key 4';
          HID_USAGE_TELEPHONY_KEY_5:
            UsageText := 'Key 5';
          HID_USAGE_TELEPHONY_KEY_6:
            UsageText := 'Key 6';
          HID_USAGE_TELEPHONY_KEY_7:
            UsageText := 'Key 7';
          HID_USAGE_TELEPHONY_KEY_8:
            UsageText := 'Key 8';
          HID_USAGE_TELEPHONY_KEY_9:
            UsageText := 'Key 9';
          HID_USAGE_TELEPHONY_KEY_STAR:
            UsageText := 'Key *';
          HID_USAGE_TELEPHONY_KEY_POUND:
            UsageText := 'Key #';
          HID_USAGE_TELEPHONY_KEY_A:
            UsageText := 'Key A';
          HID_USAGE_TELEPHONY_KEY_B:
            UsageText := 'Key B';
          HID_USAGE_TELEPHONY_KEY_C:
            UsageText := 'Key C';
          HID_USAGE_TELEPHONY_KEY_D:
            UsageText := 'Key D';
        end;
      end;
    HID_USAGE_PAGE_CONSUMER:
      begin
        UsagePageText := 'Consumer';
        case Usage of
          HID_USAGE_CONSUMER_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_CONSUMER_CONSUMER_CONTROL:
            UsageText := 'Consumer Control';
          HID_USAGE_CONSUMER_NUMERIC_KEY_PAD:
            UsageText := 'Numeric Keypad';
          HID_USAGE_CONSUMER_PROGRAMMABLE_BUTTONS:
            UsageText := 'Programmable Buttons';
          HID_USAGE_CONSUMER_MICROPHONE:
            UsageText := 'Microphone';
          HID_USAGE_CONSUMER_HEADPHONE:
            UsageText := 'Headphone';
          HID_USAGE_CONSUMER_GRAPHIC_EQUALIZER:
            UsageText := 'Graphic Equalizer';
          HID_USAGE_CONSUMER_PLUS_10:
            UsageText := '+10';
          HID_USAGE_CONSUMER_PLUS_100:
            UsageText := '+100';
          HID_USAGE_CONSUMER_AM_PM:
            UsageText := 'AM/PM';
          HID_USAGE_CONSUMER_POWER:
            UsageText := 'Power';
          HID_USAGE_CONSUMER_RESET:
            UsageText := 'Reset';
          HID_USAGE_CONSUMER_SLEEP:
            UsageText := 'Sleep';
          HID_USAGE_CONSUMER_SLEEP_AFTER:
            UsageText := 'Sleep After';
          HID_USAGE_CONSUMER_SLEEP_MODE:
            UsageText := 'Sleep Mode';
          HID_USAGE_CONSUMER_ILLUMINATION:
            UsageText := 'Illumination';
          HID_USAGE_CONSUMER_FUNCTION_BUTTONS:
            UsageText := 'Function Buttons';
          HID_USAGE_CONSUMER_MENU:
            UsageText := 'Menu';
          HID_USAGE_CONSUMER_MENU_PICK:
            UsageText := 'Menu Pick';
          HID_USAGE_CONSUMER_MENU_UP:
            UsageText := 'Menu Up';
          HID_USAGE_CONSUMER_MENU_DOWN:
            UsageText := 'Menu Down';
          HID_USAGE_CONSUMER_MENU_LEFT:
            UsageText := 'Menu Left';
          HID_USAGE_CONSUMER_MENU_RIGHT:
            UsageText := 'Menu Right';
          HID_USAGE_CONSUMER_MENU_ESCAPE:
            UsageText := 'Menu Escape';
          HID_USAGE_CONSUMER_MENU_VALUE_INCREASE:
            UsageText := 'Menu Value Increase';
          HID_USAGE_CONSUMER_MENU_VALUE_DECREASE:
            UsageText := 'Menu Value Decrease';
          HID_USAGE_CONSUMER_DATA_ON_SCREEN:
            UsageText := 'Data On Screen';
          HID_USAGE_CONSUMER_CLOSED_CAPTION:
            UsageText := 'Closed Caption';
          HID_USAGE_CONSUMER_CLOSED_CAPTION_SELECT:
            UsageText := 'Closed Caption Select';
          HID_USAGE_CONSUMER_VCR_TV:
            UsageText := 'VCR/TV';
          HID_USAGE_CONSUMER_BROADCAST_MODE:
            UsageText := 'Broadcast Mode';
          HID_USAGE_CONSUMER_SNAPSHOT:
            UsageText := 'Snapshot';
          HID_USAGE_CONSUMER_STILL:
            UsageText := 'Still';
          HID_USAGE_CONSUMER_SELECTION:
            UsageText := 'Selection';
          HID_USAGE_CONSUMER_ASSIGN_SELECTION:
            UsageText := 'Assign Selection';
          HID_USAGE_CONSUMER_MODE_STEP:
            UsageText := 'Mode Step';
          HID_USAGE_CONSUMER_RECALL_LAST:
            UsageText := 'Recall Last';
          HID_USAGE_CONSUMER_ENTER_CHANNEL:
            UsageText := 'Enter Channel';
          HID_USAGE_CONSUMER_ORDER_MOVIE:
            UsageText := 'Order Movie';
          HID_USAGE_CONSUMER_CHANNEL:
            UsageText := 'Channel';
          HID_USAGE_CONSUMER_MEDIA_SELECTION:
            UsageText := 'Media Selection';
          HID_USAGE_CONSUMER_MEDIA_SELECT_COMPUTER:
            UsageText := 'Media Select Computer';
          HID_USAGE_CONSUMER_MEDIA_SELECT_TV:
            UsageText := 'Media Select TV';
          HID_USAGE_CONSUMER_MEDIA_SELECT_WWW:
            UsageText := 'Media Select WWW';
          HID_USAGE_CONSUMER_MEDIA_SELECT_DVD:
            UsageText := 'Media Select DVD';
          HID_USAGE_CONSUMER_MEDIA_SELECT_TELEPHONE:
            UsageText := 'Media Select Telephone';
          HID_USAGE_CONSUMER_MEDIA_SELECT_PROGRAM_GUIDE:
            UsageText := 'Media Select Program Guide';
          HID_USAGE_CONSUMER_MEDIA_SELECT_VIDEO_PHONE:
            UsageText := 'Media Select Video Phone';
          HID_USAGE_CONSUMER_MEDIA_SELECT_GAMES:
            UsageText := 'Media Select Games';
          HID_USAGE_CONSUMER_MEDIA_SELECT_MESSAGES:
            UsageText := 'Media Select Messages';
          HID_USAGE_CONSUMER_MEDIA_SELECT_CD:
            UsageText := 'Media Select CD';
          HID_USAGE_CONSUMER_MEDIA_SELECT_VCR:
            UsageText := 'Media Select VCR';
          HID_USAGE_CONSUMER_MEDIA_SELECT_TUNER:
            UsageText := 'Media Select Tuner';
          HID_USAGE_CONSUMER_QUIT:
            UsageText := 'Quit';
          HID_USAGE_CONSUMER_HELP:
            UsageText := 'Help';
          HID_USAGE_CONSUMER_MEDIA_SELECT_TAPE:
            UsageText := 'Media Select Tape';
          HID_USAGE_CONSUMER_MEDIA_SELECT_CABLE:
            UsageText := 'Media Select Cable';
          HID_USAGE_CONSUMER_MEDIA_SELECT_SATELLITE:
            UsageText := 'Media Select Satellite';
          HID_USAGE_CONSUMER_MEDIA_SELECT_SECURITY:
            UsageText := 'Media Select Security';
          HID_USAGE_CONSUMER_MEDIA_SELECT_HOME:
            UsageText := 'Media Select Home';
          HID_USAGE_CONSUMER_MEDIA_SELECT_CALL:
            UsageText := 'Media Select Call';
          HID_USAGE_CONSUMER_CHANNEL_INCREMENT:
            UsageText := 'Channel Increment';
          HID_USAGE_CONSUMER_CHANNEL_DECREMENT:
            UsageText := 'Channel Decrement';
          HID_USAGE_CONSUMER_MEDIA_SELECT_SAP:
            UsageText := 'Media Select SAP';
          HID_USAGE_CONSUMER_RESERVED:
            UsageText := 'Reserved';
          HID_USAGE_CONSUMER_VCR_PLUS:
            UsageText := 'VCR Plus';
          HID_USAGE_CONSUMER_ONCE:
            UsageText := 'Once';
          HID_USAGE_CONSUMER_DAILY:
            UsageText := 'Daily';
          HID_USAGE_CONSUMER_WEEKLY:
            UsageText := 'Weekly';
          HID_USAGE_CONSUMER_MONTHLY:
            UsageText := 'Monthly';
          HID_USAGE_CONSUMER_PLAY:
            UsageText := 'Play';
          HID_USAGE_CONSUMER_PAUSE:
            UsageText := 'Pause';
          HID_USAGE_CONSUMER_RECORD:
            UsageText := 'Record';
          HID_USAGE_CONSUMER_FAST_FORWARD:
            UsageText := 'Fast Forward';
          HID_USAGE_CONSUMER_REWIND:
            UsageText := 'Rewind';
          HID_USAGE_CONSUMER_SCAN_NEXT_TRACK:
            UsageText := 'Scan Next Track';
          HID_USAGE_CONSUMER_SCAN_PREV_TRACK:
            UsageText := 'Scan Previous Track';
          HID_USAGE_CONSUMER_STOP:
            UsageText := 'Stop';
          HID_USAGE_CONSUMER_EJECT:
            UsageText := 'Eject';
          HID_USAGE_CONSUMER_RANDOM_PLAY:
            UsageText := 'Random Play';
          HID_USAGE_CONSUMER_SELECT_DISC:
            UsageText := 'Select Disc';
          HID_USAGE_CONSUMER_ENTER_DISC:
            UsageText := 'Enter Disc';
          HID_USAGE_CONSUMER_REPEAT:
            UsageText := 'Repeat';
          HID_USAGE_CONSUMER_TRACKING:
            UsageText := 'Tracking';
          HID_USAGE_CONSUMER_TRACK_NORMAL:
            UsageText := 'Track Normal';
          HID_USAGE_CONSUMER_SLOW_TRACKING:
            UsageText := 'Slow Tracking';
          HID_USAGE_CONSUMER_FRAME_FORWARD:
            UsageText := 'Frame Forward';
          HID_USAGE_CONSUMER_FRAME_BACK:
            UsageText := 'Frame Back';
          HID_USAGE_CONSUMER_MARK:
            UsageText := 'Mark';
          HID_USAGE_CONSUMER_CLEAR_MARK:
            UsageText := 'Clear Mark';
          HID_USAGE_CONSUMER_REPEAT_FROM_MARK:
            UsageText := 'Repeat from Mark';
          HID_USAGE_CONSUMER_RETURN_TO_MARK:
            UsageText := 'Return to Mark';
          HID_USAGE_CONSUMER_SEARCH_MARK_FORWARD:
            UsageText := 'Search Mark Forward';
          HID_USAGE_CONSUMER_SEARCK_MARK_BACKWARDS:
            UsageText := 'Search Mark Backwards';
          HID_USAGE_CONSUMER_COUNTER_RESET:
            UsageText := 'Counter Reset';
          HID_USAGE_CONSUMER_SHOW_COUNTER:
            UsageText := 'Show Counter';
          HID_USAGE_CONSUMER_TRACKING_INCREMENT:
            UsageText := 'Tracking Increment';
          HID_USAGE_CONSUMER_TRACKING_DECREMENT:
            UsageText := 'Tracking Decrement';
          HID_USAGE_CONSUMER_STOP_EJECT:
            UsageText := 'Stop/Eject';
          HID_USAGE_CONSUMER_PLAY_PAUSE:
            UsageText := 'Play/Pause';
          HID_USAGE_CONSUMER_PLAY_SKIP:
            UsageText := 'Play/Skip';
          HID_USAGE_CONSUMER_VOLUME:
            UsageText := 'Volume';
          HID_USAGE_CONSUMER_BALANCE:
            UsageText := 'Balance';
          HID_USAGE_CONSUMER_MUTE:
            UsageText := 'Mute';
          HID_USAGE_CONSUMER_BASS:
            UsageText := 'Bass';
          HID_USAGE_CONSUMER_TREBLE:
            UsageText := 'Treble';
          HID_USAGE_CONSUMER_BASS_BOOST:
            UsageText := 'Bass Boost';
          HID_USAGE_CONSUMER_SURROUND_MODE:
            UsageText := 'Surround Mode';
          HID_USAGE_CONSUMER_LOUDNESS:
            UsageText := 'Loudness';
          HID_USAGE_CONSUMER_MPX:
            UsageText := 'MPX';
          HID_USAGE_CONSUMER_VOLUME_INCREMENT:
            UsageText := 'Volume Increment';
          HID_USAGE_CONSUMER_VOLUME_DECREMENT:
            UsageText := 'Volume Decrement';
          HID_USAGE_CONSUMER_SPEED_SELECT:
            UsageText := 'Speed Select';
          HID_USAGE_CONSUMER_PLAYBACK_SPEED:
            UsageText := 'Playback Speed';
          HID_USAGE_CONSUMER_STANDARD_PLAY:
            UsageText := 'Standard Play';
          HID_USAGE_CONSUMER_LONG_PLAY:
            UsageText := 'Long Play';
          HID_USAGE_CONSUMER_EXTENDED_PLAY:
            UsageText := 'Extended Play';
          HID_USAGE_CONSUMER_SLOW:
            UsageText := 'Slow';
          HID_USAGE_CONSUMER_FAN_ENABLE:
            UsageText := 'Fan Enable';
          HID_USAGE_CONSUMER_FAN_SPEED:
            UsageText := 'Fan Speed';
          HID_USAGE_CONSUMER_LIGHT_ENABLE:
            UsageText := 'Light Enable';
          HID_USAGE_CONSUMER_LIGHT_ILLUMINATION_LEVEL:
            UsageText := 'Light Illumination Level';
          HID_USAGE_CONSUMER_CLIMATE_CONTROL_ENABLE:
            UsageText := 'Climate Control Enable';
          HID_USAGE_CONSUMER_ROOM_TEMPERATURE:
            UsageText := 'Room Temperature';
          HID_USAGE_CONSUMER_SECURITY_ENABLE:
            UsageText := 'Security Enable';
          HID_USAGE_CONSUMER_FIRE_ALARM:
            UsageText := 'Fire Alarm';
          HID_USAGE_CONSUMER_POLICE_ALARM:
            UsageText := 'Police Alarm';
          HID_USAGE_CONSUMER_PROXIMITY:
            UsageText := 'Proximity';
          HID_USAGE_CONSUMER_MOTION:
            UsageText := 'Motion';
          HID_USAGE_CONSUMER_DURESS_ALARM:
            UsageText := 'Duress Alarm';
          HID_USAGE_CONSUMER_HOLDUP_ALARM:
            UsageText := 'Holdup Alarm';
          HID_USAGE_CONSUMER_MEDICAL_ALARM:
            UsageText := 'Medical Alarm';
          HID_USAGE_CONSUMER_BALANCE_RIGHT:
            UsageText := 'Balance Right';
          HID_USAGE_CONSUMER_BALANCE_LEFT:
            UsageText := 'Balance Left';
          HID_USAGE_CONSUMER_BASS_INCREMENT:
            UsageText := 'Bass Increment';
          HID_USAGE_CONSUMER_BASS_DECREMENT:
            UsageText := 'Bass Decrement';
          HID_USAGE_CONSUMER_TREBLE_INCREMENT:
            UsageText := 'Treble Increment';
          HID_USAGE_CONSUMER_TREBLE_DECREMENT:
            UsageText := 'Treble Decrement';
          HID_USAGE_CONSUMER_SPEAKER_SYSTEM:
            UsageText := 'Speaker System';
          HID_USAGE_CONSUMER_CHANNEL_LEFT:
            UsageText := 'Channel Left';
          HID_USAGE_CONSUMER_CHANNEL_RIGHT:
            UsageText := 'Channel Right';
          HID_USAGE_CONSUMER_CHANNEL_CENTER:
            UsageText := 'Channel Center';
          HID_USAGE_CONSUMER_CHANNEL_FRONT:
            UsageText := 'Channel Front';
          HID_USAGE_CONSUMER_CHANNEL_CENTER_FRONT:
            UsageText := 'Channel Center Front';
          HID_USAGE_CONSUMER_CHANNEL_SIDE:
            UsageText := 'Channel Side';
          HID_USAGE_CONSUMER_CHANNEL_SURROUND:
            UsageText := 'Channel Surround';
          HID_USAGE_CONSUMER_CHANNEL_LOW_FREQ_ENH:
            UsageText := 'Channel Low Frequency Enhancement';
          HID_USAGE_CONSUMER_CHANNEL_TOP:
            UsageText := 'Channel Top';
          HID_USAGE_CONSUMER_CHANNEL_UNKNOWN:
            UsageText := 'Channel Unknown';
          HID_USAGE_CONSUMER_SUB_CHANNEL:
            UsageText := 'Sub-channel';
          HID_USAGE_CONSUMER_SUB_CHANNEL_INCREMENT:
            UsageText := 'Sub-channel Increment';
          HID_USAGE_CONSUMER_SUB_CHANNEL_DECREMENT:
            UsageText := 'Sub-channel Decrement';
          HID_USAGE_CONSUMER_ALTERNATE_AUDIO_INCREMENT:
            UsageText := 'Alternate Audio Increment';
          HID_USAGE_CONSUMER_ALTERNATE_AUDIO_DECREMENT:
            UsageText := 'Alternate Audio Decrement';
          HID_USAGE_CONSUMER_APP_LAUNCH_BUTTONS:
            UsageText := 'Application Launch Buttons';
          HID_USAGE_CONSUMER_AL_LAUNCH_BUTTON_CONFIG_TOOL:
            UsageText := 'Application Launch Button Configuration Tool';
          HID_USAGE_CONSUMER_AL_PROG_BUTTON_CONFIG:
            UsageText := 'Application Launch Programmable Button Configuration';
          HID_USAGE_CONSUMER_AL_CONSUMER_CONTROL_CONFIG:
            UsageText := 'Application Launch Consumer Control Configuration';
          HID_USAGE_CONSUMER_AL_WORD_PROCESSOR:
            UsageText := 'Application Launch Word Processor';
          HID_USAGE_CONSUMER_AL_TEXT_EDITOR:
            UsageText := 'Application Launch Text Editor';
          HID_USAGE_CONSUMER_AL_SPREADSHEET:
            UsageText := 'Application Launch Spreadsheet';
          HID_USAGE_CONSUMER_AL_GRAPHICS_EDITOR:
            UsageText := 'Application Launch Graphics Editor';
          HID_USAGE_CONSUMER_AL_PRESENTATION_APP:
            UsageText := 'Application Launch Presentation Application';
          HID_USAGE_CONSUMER_AL_DATABASE_APP:
            UsageText := 'Application Launch Database Application';
          HID_USAGE_CONSUMER_AL_EMAIL_READER:
            UsageText := 'Application Launch Email Reader';
          HID_USAGE_CONSUMER_AL_NEWSREADER:
            UsageText := 'Application Launch Newsreader';
          HID_USAGE_CONSUMER_AL_VOICEMAIL:
            UsageText := 'Application Launch Voicemail';
          HID_USAGE_CONSUMER_AL_CONTACTS_ADDESSBOOK:
            UsageText := 'Application Launch Contacts/Addressbook';
          HID_USAGE_CONSUMER_AL_CALENDAR_SCHEDULE:
            UsageText := 'Application Launch Calendar/Schedule';
          HID_USAGE_CONSUMER_AL_TASK_PROJECT_MANAGER:
            UsageText := 'Application Launch Task/Project Manager';
          HID_USAGE_CONSUMER_AL_LOG_JOURNAL_TIMECARD:
            UsageText := 'Application Launch Log/Journal/Timecard';
          HID_USAGE_CONSUMER_AL_CHECKBOOK_FINANCE:
            UsageText := 'Application Launch Checkbook/Finance';
          HID_USAGE_CONSUMER_AL_CALCULATOR:
            UsageText := 'Application Launch Calculator';
          HID_USAGE_CONSUMER_AL_AV_CAPTURE_PLAYBACK:
            UsageText := 'Application Launch A/V Capture/Playback';
          HID_USAGE_CONSUMER_AL_LOCAL_MACHINE_BROWSER:
            UsageText := 'Application Launch Local Machine Browser';
          HID_USAGE_CONSUMER_AL_LAN_WAN_BROWSER:
            UsageText := 'Application Launch LAN/WAN Browser';
          HID_USAGE_CONSUMER_AL_INTERNET_BROWSER:
            UsageText := 'Application Launch Internet Browser';
          HID_USAGE_CONSUMER_AL_REMOTE_NETWORKING_ISP_CONNECT:
            UsageText := 'Application Launch Remote Networking/ISP Connect';
          HID_USAGE_CONSUMER_AL_NETWORK_CONFERENCE:
            UsageText := 'Application Launch Network Conference';
          HID_USAGE_CONSUMER_AL_NETWORK_CHAT:
            UsageText := 'Application Launch Network Chat';
          HID_USAGE_CONSUMER_AL_TELEPHONY_DIALER:
            UsageText := 'Application Launch Telephony/Dialer';
          HID_USAGE_CONSUMER_AL_LOGON:
            UsageText := 'Application Launch Logon';
          HID_USAGE_CONSUMER_AL_LOGOFF:
            UsageText := 'Application Launch Logoff';
          HID_USAGE_CONSUMER_AL_LOGON_LOGOFF:
            UsageText := 'Application Launch Logon/Logoff';
          HID_USAGE_CONSUMER_AL_TERMINAL_LOCK_SCREENSAVER:
            UsageText := 'Application Launch Terminal Lock/Screensaver';
          HID_USAGE_CONSUMER_AL_CONTROL_PANEL:
            UsageText := 'Application Launch Control Panel';
          HID_USAGE_CONSUMER_AL_COMMAND_LINE_PROCESSOR_RUN:
            UsageText := 'Application Launch Command Line Processor/Run';
          HID_USAGE_CONSUMER_AL_PROCESS_TASK_MANAGER:
            UsageText := 'Application Launch Process/Task Manager';
          HID_USAGE_CONSUMER_AL_SELECT_TASK_APP:
            UsageText := 'Application Launch Select Task/Application';
          HID_USAGE_CONSUMER_AL_NEXT_TASK_APP:
            UsageText := 'Application Launch Next Task/Application';
          HID_USAGE_CONSUMER_AL_PREV_TASK_APP:
            UsageText := 'Application Launch Previous Task/Application';
          HID_USAGE_CONSUMER_AL_PREEMPTIVE_HALT_TASK_APP:
            UsageText := 'Application Launch Preemptive Halt Task/Application';
          HID_USAGE_CONSUMER_AL_INTEGRATED_HELP_CENTER:
            UsageText := 'Application Launch Integrated Help Center';
          HID_USAGE_CONSUMER_AL_DOCUMENTS:
            UsageText := 'Application Launch Documents';
          HID_USAGE_CONSUMER_AL_THESAURUS:
            UsageText := 'Application Launch Thesaurus';
          HID_USAGE_CONSUMER_AL_DICTIONARY:
            UsageText := 'Application Launch Dictionary';
          HID_USAGE_CONSUMER_AL_DESKTOP:
            UsageText := 'Application Launch Desktop';
          HID_USAGE_CONSUMER_AL_SPELL_CHECK:
            UsageText := 'Application Launch Spell Check';
          HID_USAGE_CONSUMER_AL_GRAMMAR_CHECK:
            UsageText := 'Application Launch Grammar Check';
          HID_USAGE_CONSUMER_AL_WIRELESS_STATUS:
            UsageText := 'Application Launch Wireless Status';
          HID_USAGE_CONSUMER_AL_KEYBOARD_LAYOUT:
            UsageText := 'Application Launch Keyboard Layout';
          HID_USAGE_CONSUMER_AL_VIRUS_PROTECTION:
            UsageText := 'Application Launch Virus Protection';
          HID_USAGE_CONSUMER_AL_ENCRYPTION:
            UsageText := 'Application Launch Encryption';
          HID_USAGE_CONSUMER_AL_SCREENSAVER:
            UsageText := 'Application Launch Screensaver';
          HID_USAGE_CONSUMER_AL_ALARMS:
            UsageText := 'Application Launch Alarms';
          HID_USAGE_CONSUMER_AL_CLOCK:
            UsageText := 'Application Launch Clock';
          HID_USAGE_CONSUMER_AL_FILE_BROWSER:
            UsageText := 'Application Launch File Browser';
          HID_USAGE_CONSUMER_AL_POWER_STATUS:
            UsageText := 'Application Launch Power Status';
          HID_USAGE_CONSUMER_GENERIC_GUI_APP_CONTROLS:
            UsageText := 'Generic GUI Application Controls';
          HID_USAGE_CONSUMER_AC_NEW:
            UsageText := 'Application Control New';
          HID_USAGE_CONSUMER_AC_OPEN:
            UsageText := 'Application Control Open';
          HID_USAGE_CONSUMER_AC_CLOSE:
            UsageText := 'Application Control Cose';
          HID_USAGE_CONSUMER_AC_EXIT:
            UsageText := 'Application Control Exit';
          HID_USAGE_CONSUMER_AC_MAXIMIZE:
            UsageText := 'Application Control Maximize';
          HID_USAGE_CONSUMER_AC_MINIMIZE:
            UsageText := 'Application Control Minimize';
          HID_USAGE_CONSUMER_AC_SAVE:
            UsageText := 'Application Control Save';
          HID_USAGE_CONSUMER_AC_PRINT:
            UsageText := 'Application Control Print';
          HID_USAGE_CONSUMER_AC_PROPERTIES:
            UsageText := 'Application Control Properties';
          HID_USAGE_CONSUMER_AC_UNDO:
            UsageText := 'Application Control Undo';
          HID_USAGE_CONSUMER_AC_COPY:
            UsageText := 'Application Control Copy';
          HID_USAGE_CONSUMER_AC_CUT:
            UsageText := 'Application Control Cut';
          HID_USAGE_CONSUMER_AC_PASTE:
            UsageText := 'Application Control Paste';
          HID_USAGE_CONSUMER_AC_SELECT_ALL:
            UsageText := 'Application Control Select All';
          HID_USAGE_CONSUMER_AC_FIND:
            UsageText := 'Application Control Find';
          HID_USAGE_CONSUMER_AC_FIND_AND_REPLACE:
            UsageText := 'Application Control Find and Replace';
          HID_USAGE_CONSUMER_AC_SEARCH:
            UsageText := 'Application Control Search';
          HID_USAGE_CONSUMER_AC_GO_TO:
            UsageText := 'Application Control Go To';
          HID_USAGE_CONSUMER_AC_HOME:
            UsageText := 'Application Control Home';
          HID_USAGE_CONSUMER_AC_BACK:
            UsageText := 'Application Control Back';
          HID_USAGE_CONSUMER_AC_FORWARD:
            UsageText := 'Application Control Forward';
          HID_USAGE_CONSUMER_AC_STOP:
            UsageText := 'Application Control Stop';
          HID_USAGE_CONSUMER_AC_REFRESH:
            UsageText := 'Application Control Refresh';
          HID_USAGE_CONSUMER_AC_PREV_LINK:
            UsageText := 'Application Control Previous Link';
          HID_USAGE_CONSUMER_AC_NEXT_LINK:
            UsageText := 'Application Control Next Link';
          HID_USAGE_CONSUMER_AC_BOOKMARKS:
            UsageText := 'Application Control Bookmarks';
          HID_USAGE_CONSUMER_AC_HISTORY:
            UsageText := 'Application Control History';
          HID_USAGE_CONSUMER_AC_SUBSCRIPTIONS:
            UsageText := 'Application Control Subscriptions';
          HID_USAGE_CONSUMER_AC_ZOOM_IN:
            UsageText := 'Application Control Zoom In';
          HID_USAGE_CONSUMER_AC_ZOOM_OUT:
            UsageText := 'Application Control Zoom Out';
          HID_USAGE_CONSUMER_AC_ZOOM:
            UsageText := 'Application Control Zoom';
          HID_USAGE_CONSUMER_AC_FULL_SCREEN_VIEW:
            UsageText := 'Application Control Full Screen View';
          HID_USAGE_CONSUMER_AC_NORMAL_VIEW:
            UsageText := 'Application Control Normal View';
          HID_USAGE_CONSUMER_AC_VIEW_TOGGLE:
            UsageText := 'Application Control View Toggle';
          HID_USAGE_CONSUMER_AC_SCROLL_UP:
            UsageText := 'Application Control Scroll Up';
          HID_USAGE_CONSUMER_AC_SCROLL_DOWN:
            UsageText := 'Application Control Scroll Down';
          HID_USAGE_CONSUMER_AC_SCROLL:
            UsageText := 'Application Control Scroll';
          HID_USAGE_CONSUMER_AC_PAN_LEFT:
            UsageText := 'Application Control Pan Left';
          HID_USAGE_CONSUMER_AC_PAN_RIGHT:
            UsageText := 'Application Control Pan Right';
          HID_USAGE_CONSUMER_AC_PAN:
            UsageText := 'Application Control Pan';
          HID_USAGE_CONSUMER_AC_NEW_WINDOW:
            UsageText := 'Application Control New Window';
          HID_USAGE_CONSUMER_AC_TILE_HORIZONTALLY:
            UsageText := 'Application Control Tile Horizontally';
          HID_USAGE_CONSUMER_AC_TILE_VERTICALLY:
            UsageText := 'Application Control Tile Vertically';
          HID_USAGE_CONSUMER_AC_FORMAT:
            UsageText := 'Application Control Format';
          HID_USAGE_CONSUMER_AC_EDIT:
            UsageText := 'Application Control Edit';
          HID_USAGE_CONSUMER_AC_BOLD:
            UsageText := 'Application Control Bold';
          HID_USAGE_CONSUMER_AC_ITALICS:
            UsageText := 'Application Control Italics';
          HID_USAGE_CONSUMER_AC_UNDERLINE:
            UsageText := 'Application Control Underline';
          HID_USAGE_CONSUMER_AC_STRIKETHROUGH:
            UsageText := 'Application Control Strikethrough';
          HID_USAGE_CONSUMER_AC_SUBSCRIPT:
            UsageText := 'Application Control Subscript';
          HID_USAGE_CONSUMER_AC_SUPERSCRIPT:
            UsageText := 'Application Control Superscript';
          HID_USAGE_CONSUMER_AC_ALL_CAPS:
            UsageText := 'Application Control All Caps';
          HID_USAGE_CONSUMER_AC_ROTATE:
            UsageText := 'Application Control Rotate';
          HID_USAGE_CONSUMER_AC_RESIZE:
            UsageText := 'Application Control Resize';
          HID_USAGE_CONSUMER_AC_FLIP_HORIZONTAL:
            UsageText := 'Application Control Flip Horizontal';
          HID_USAGE_CONSUMER_AC_FLIP_VERTICAL:
            UsageText := 'Application Control Flip Vertical';
          HID_USAGE_CONSUMER_AC_MIRROR_HORIZONTAL:
            UsageText := 'Application Control Mirror Horizontal';
          HID_USAGE_CONSUMER_AC_MIRROR_VERTICAL:
            UsageText := 'Application Control Mirror Vertical';
          HID_USAGE_CONSUMER_AC_FONT_SELECT:
            UsageText := 'Application Control Font Select';
          HID_USAGE_CONSUMER_AC_FONT_COLOR:
            UsageText := 'Application Control Font Color';
          HID_USAGE_CONSUMER_AC_FONT_SIZE:
            UsageText := 'Application Control Font Size';
          HID_USAGE_CONSUMER_AC_JUSTIFY_LEFT:
            UsageText := 'Application Control Justify Left';
          HID_USAGE_CONSUMER_AC_JUSTIFY_CENTER_H:
            UsageText := 'Application Control Justify Center Horizontally';
          HID_USAGE_CONSUMER_AC_JUSTIFY_RIGHT:
            UsageText := 'Application Control Justify Right';
          HID_USAGE_CONSUMER_AC_JUSTIFY_BLOCK_H:
            UsageText := 'Application Control Justify Block Horizontally';
          HID_USAGE_CONSUMER_AC_JUSTIFY_TOP:
            UsageText := 'Application Control Justify Top';
          HID_USAGE_CONSUMER_AC_JUSTIFY_CENTER_V:
            UsageText := 'Application Control Justify Center Vertically';
          HID_USAGE_CONSUMER_AC_JUSTIFY_BOTTOM:
            UsageText := 'Application Control Justify Bottom';
          HID_USAGE_CONSUMER_AC_JUSTIFY_BLOCK_V:
            UsageText := 'Application Control Justify Block Vertically';
          HID_USAGE_CONSUMER_AC_INDENT_DECREASE:
            UsageText := 'Application Control Indent Decrease';
          HID_USAGE_CONSUMER_AC_INDENT_INCREASE:
            UsageText := 'Application Control Indent Increase';
          HID_USAGE_CONSUMER_AC_NUMBERED_LIST:
            UsageText := 'Application Control Numbered List';
          HID_USAGE_CONSUMER_AC_RESTART_NUMBERING:
            UsageText := 'Application Control Restart Numbering';
          HID_USAGE_CONSUMER_AC_BULLETED_LIST:
            UsageText := 'Application Control Bulleted List';
          HID_USAGE_CONSUMER_AC_PROMOTE:
            UsageText := 'Application Control Promote';
          HID_USAGE_CONSUMER_AC_DEMOTE:
            UsageText := 'Application Control Demote';
          HID_USAGE_CONSUMER_AC_YES:
            UsageText := 'Application Control Yes';
          HID_USAGE_CONSUMER_AC_NO:
            UsageText := 'Application Control No';
          HID_USAGE_CONSUMER_AC_CANCEL:
            UsageText := 'Application Control Cancel';
          HID_USAGE_CONSUMER_AC_CATALOG:
            UsageText := 'Application Control Catalog';
          HID_USAGE_CONSUMER_AC_BUY_CHECKOUT:
            UsageText := 'Application Control Buy Checkout';
          HID_USAGE_CONSUMER_AC_ADD_TO_CART:
            UsageText := 'Application Control Add To Cart';
          HID_USAGE_CONSUMER_AC_EXPAND:
            UsageText := 'Application Control Expand';
          HID_USAGE_CONSUMER_AC_EXPAND_ALL:
            UsageText := 'Application Control Expand All';
          HID_USAGE_CONSUMER_AC_COLLAPSE:
            UsageText := 'Application Control Collapse';
          HID_USAGE_CONSUMER_AC_COLLAPSE_ALL:
            UsageText := 'Application Control Collapse All';
          HID_USAGE_CONSUMER_AC_PRINT_PREVIEW:
            UsageText := 'Application Control Print Preview';
          HID_USAGE_CONSUMER_AC_PASTE_SPECIAL:
            UsageText := 'Application Control Paste Special';
          HID_USAGE_CONSUMER_AC_INSERT_MODE:
            UsageText := 'Application Control Insert Mode';
          HID_USAGE_CONSUMER_AC_DELETE:
            UsageText := 'Application Control Delete';
          HID_USAGE_CONSUMER_AC_LOCK:
            UsageText := 'Application Control Lock';
          HID_USAGE_CONSUMER_AC_UNLOCK:
            UsageText := 'Application Control Unlock';
          HID_USAGE_CONSUMER_AC_PROTECT:
            UsageText := 'Application Control Protect';
          HID_USAGE_CONSUMER_AC_UNPROTECT:
            UsageText := 'Application Control Unprotect';
          HID_USAGE_CONSUMER_AC_ATTACH_COMMENT:
            UsageText := 'Application Control Attach Comment';
          HID_USAGE_CONSUMER_AC_DELETE_COMMENT:
            UsageText := 'Application Control Delete Comment';
          HID_USAGE_CONSUMER_AC_VIEW_COMMENT:
            UsageText := 'Application Control View Comment';
          HID_USAGE_CONSUMER_AC_SELECT_WORD:
            UsageText := 'Application Control Select Word';
          HID_USAGE_CONSUMER_AC_SELECT_SENTENCE:
            UsageText := 'Application Control Select Sentence';
          HID_USAGE_CONSUMER_AC_SELECT_PARAGRAPH:
            UsageText := 'Application Control Select Paragraph';
          HID_USAGE_CONSUMER_AC_SELECT_COLUMN:
            UsageText := 'Application Control Select Column';
          HID_USAGE_CONSUMER_AC_SELECT_ROW:
            UsageText := 'Application Control Select Row';
          HID_USAGE_CONSUMER_AC_SELECT_TABLE:
            UsageText := 'Application Control Select Table';
          HID_USAGE_CONSUMER_AC_SELECT_OBJECT:
            UsageText := 'Application Control Select Object';
          HID_USAGE_CONSUMER_AC_REDO_REPEAT:
            UsageText := 'Application Control Redo/Repeat';
          HID_USAGE_CONSUMER_AC_SORT:
            UsageText := 'Application Control Sort';
          HID_USAGE_CONSUMER_AC_SORT_ASCENDING:
            UsageText := 'Application Control Sort Ascending';
          HID_USAGE_CONSUMER_AC_SORT_DESCENDING:
            UsageText := 'Application Control Sort Descending';
          HID_USAGE_CONSUMER_AC_FILTER:
            UsageText := 'Application Control Filter';
          HID_USAGE_CONSUMER_AC_SET_CLOCK:
            UsageText := 'Application Control Set Clock';
          HID_USAGE_CONSUMER_AC_VIEW_CLOCK:
            UsageText := 'Application Control View Clock';
          HID_USAGE_CONSUMER_AC_SELECT_TIME_ZONE:
            UsageText := 'Application Control Select Time Zone';
          HID_USAGE_CONSUMER_AC_EDIT_TIME_ZONES:
            UsageText := 'Application Control Edit Time Zones';
          HID_USAGE_CONSUMER_AC_SET_ALARM:
            UsageText := 'Application Control Set Alarm';
          HID_USAGE_CONSUMER_AC_CLEAR_ALARM:
            UsageText := 'Application Control Clear Alarm';
          HID_USAGE_CONSUMER_AC_SNOOZE_ALARM:
            UsageText := 'Application Control Snooze Alarm';
          HID_USAGE_CONSUMER_AC_RESET_ALARM:
            UsageText := 'Application Control Reset Alarm';
          HID_USAGE_CONSUMER_AC_SYNCHRONIZE:
            UsageText := 'Application Control Synchronize';
          HID_USAGE_CONSUMER_AC_SEND_RECEIVE:
            UsageText := 'Application Control Send/Receive';
          HID_USAGE_CONSUMER_AC_SEND_TO:
            UsageText := 'Application Control Send To';
          HID_USAGE_CONSUMER_AC_REPLY:
            UsageText := 'Application Control Reply';
          HID_USAGE_CONSUMER_AC_REPLY_ALL:
            UsageText := 'Application Control Reply All';
          HID_USAGE_CONSUMER_AC_FORWARD_MSG:
            UsageText := 'Application Control Forward Message';
          HID_USAGE_CONSUMER_AC_SEND:
            UsageText := 'Application Control Send';
          HID_USAGE_CONSUMER_AC_ATTACH_FILE:
            UsageText := 'Application Control Attach File';
          HID_USAGE_CONSUMER_AC_UPLOAD:
            UsageText := 'Application Control Upload';
          HID_USAGE_CONSUMER_AC_DOWNLOAD:
            UsageText := 'Application Control Download';
          HID_USAGE_CONSUMER_AC_SET_BORDERS:
            UsageText := 'Application Control Set Borders';
          HID_USAGE_CONSUMER_AC_INSERT_ROW:
            UsageText := 'Application Control Insert Row';
          HID_USAGE_CONSUMER_AC_INSERT_COLUMN:
            UsageText := 'Application Control Insert Column';
          HID_USAGE_CONSUMER_AC_INSERT_FILE:
            UsageText := 'Application Control Insert File';
          HID_USAGE_CONSUMER_AC_INSERT_PICTURE:
            UsageText := 'Application Control Insert Picture';
          HID_USAGE_CONSUMER_AC_INSERT_OBJECT:
            UsageText := 'Application Control Insert Object';
          HID_USAGE_CONSUMER_AC_INSERT_SYMBOL:
            UsageText := 'Application Control Insert Symbol';
          HID_USAGE_CONSUMER_AC_SAVE_AND_CLOSE:
            UsageText := 'Application Control Save and Close';
          HID_USAGE_CONSUMER_AC_RENAME:
            UsageText := 'Application Control Rename';
          HID_USAGE_CONSUMER_AC_MERGE:
            UsageText := 'Application Control Merge';
          HID_USAGE_CONSUMER_AC_SPLIT:
            UsageText := 'Application Control Split';
          HID_USAGE_CONSUMER_AC_DISTRIBUTE_HORIZONTALLY:
            UsageText := 'Application Control Distribute Horizontally';
          HID_USAGE_CONSUMER_AC_DISTRIBUTE_VERTICALLY:
            UsageText := 'Application Control Distribute Vertically';
        end;
      end;
    HID_USAGE_PAGE_DIGITIZER:
      begin
        UsagePageText := 'Digitizer';
        case Usage of
          HID_USAGE_DIGITIZER_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_DIGITIZER_DIGITIZER:
            UsageText := 'Digitizer';
          HID_USAGE_DIGITIZER_PEN:
            UsageText := 'Pen';
          HID_USAGE_DIGITIZER_LIGHT_PEN:
            UsageText := 'Light Pen';
          HID_USAGE_DIGITIZER_TOUCH_SCREEN:
            UsageText := 'Touch Screen';
          HID_USAGE_DIGITIZER_TOUCH_PAD:
            UsageText := 'Touch Pad';
          HID_USAGE_DIGITIZER_WHITE_BOARD:
            UsageText := 'White Board';
          HID_USAGE_DIGITIZER_COORDINATE_MEASURING_MACHINE:
            UsageText := 'Coordinate Measuring Machine';
          HID_USAGE_DIGITIZER_3D_DIGITIZER:
            UsageText := '3D Digitizer';
          HID_USAGE_DIGITIZER_STEREO_PLOTTER:
            UsageText := 'Stereo Plotter';
          HID_USAGE_DIGITIZER_ARTICULATED_ARM:
            UsageText := 'Articulated Arm';
          HID_USAGE_DIGITIZER_ARMATURE:
            UsageText := 'Armature';
          HID_USAGE_DIGITIZER_MULTIPLE_POINT_DIGITIZER:
            UsageText := 'Multiple Point Digitizer';
          HID_USAGE_DIGITIZER_FREE_SPACE_WAND:
            UsageText := 'Free Space Wand';
          HID_USAGE_DIGITIZER_STYLUS:
            UsageText := 'Stylus';
          HID_USAGE_DIGITIZER_PUCK:
            UsageText := 'Puck';
          HID_USAGE_DIGITIZER_FINGER:
            UsageText := 'Finger';
          HID_USAGE_DIGITIZER_TIP_PRESSURE:
            UsageText := 'Tip Pressure';
          HID_USAGE_DIGITIZER_BARREL_PRESSURE:
            UsageText := 'Barrel Pressure';
          HID_USAGE_DIGITIZER_IN_RANGE:
            UsageText := 'In Range';
          HID_USAGE_DIGITIZER_TOUCH:
            UsageText := 'Touch';
          HID_USAGE_DIGITIZER_UNTOUCH:
            UsageText := 'Untouch';
          HID_USAGE_DIGITIZER_TAP:
            UsageText := 'Tap';
          HID_USAGE_DIGITIZER_QUALITY:
            UsageText := 'Quality';
          HID_USAGE_DIGITIZER_DATA_VALID:
            UsageText := 'Data Valid';
          HID_USAGE_DIGITIZER_TRANSDUCER_INDEX:
            UsageText := 'Transducer Index';
          HID_USAGE_DIGITIZER_TABLET_FUNCTION_KEYS:
            UsageText := 'Tablet Function Keys';
          HID_USAGE_DIGITIZER_PROGRAM_CHANGE_KEYS:
            UsageText := 'Program Change Keys';
          HID_USAGE_DIGITIZER_BATTERY_STRENGTH:
            UsageText := 'Battery Strength';
          HID_USAGE_DIGITIZER_INVERT:
            UsageText := 'Invert';
          HID_USAGE_DIGITIZER_X_TILT:
            UsageText := 'X Tilt';
          HID_USAGE_DIGITIZER_Y_TILT:
            UsageText := 'Y Tilt';
          HID_USAGE_DIGITIZER_AZIMUTH:
            UsageText := 'Azimuth';
          HID_USAGE_DIGITIZER_ALTITUDE:
            UsageText := 'Altitude';
          HID_USAGE_DIGITIZER_TWIST:
            UsageText := 'Twist';
          HID_USAGE_DIGITIZER_TIP_SWITCH:
            UsageText := 'Tip Switch';
          HID_USAGE_DIGITIZER_SECONDARY_TIP_SWITCH:
            UsageText := 'Secondary Tip Switch';
          HID_USAGE_DIGITIZER_BARREL_SWITCH:
            UsageText := 'Barrel Switch';
          HID_USAGE_DIGITIZER_ERASER:
            UsageText := 'Eraser';
          HID_USAGE_DIGITIZER_TABLET_PICK:
            UsageText := 'Tablet Pick';
        end;
      end;
    HID_USAGE_PAGE_PHYSICAL_INPUT_DEVICE:
      begin
        UsagePageText := 'Physical Input Device (Force Feedback)';
        case Usage of
          HID_USAGE_PID_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_PID_PHYSICAL_INTERFACE_DEVICE:
            UsageText := 'Physical Interface Device';
          HID_USAGE_PID_NORMAL:
            UsageText := 'Normal';
          HID_USAGE_PID_SET_EFFECT_REPORT:
            UsageText := 'Set Effect Report';
          HID_USAGE_PID_EFFECT_BLOCK_INDEX:
            UsageText := 'Effect Block Index';
          HID_USAGE_PID_PARAMETER_BLOCK_OFFSET:
            UsageText := 'Parameter Block Offset';
          HID_USAGE_PID_ROM_FLAG:
            UsageText := 'ROM Flag';
          HID_USAGE_PID_EFFECT_TYPE:
            UsageText := 'Effect Type';
          HID_USAGE_PID_ET_CONSTANT_FORCE:
            UsageText := 'Effect Type Constant Force';
          HID_USAGE_PID_ET_RAMP:
            UsageText := 'Effect Type Ramp';
          HID_USAGE_PID_ET_CUSTOM_FORCE_DATA:
            UsageText := 'Effect Type Custom Force Data';
          HID_USAGE_PID_ET_SQUARE:
            UsageText := 'Effect Type Square';
          HID_USAGE_PID_ET_SINE:
            UsageText := 'Effect Type Sine';
          HID_USAGE_PID_ET_TRIANGLE:
            UsageText := 'Effect Type Triangle';
          HID_USAGE_PID_ET_SAWTOOTH_UP:
            UsageText := 'Effect Type Sawtooth Up';
          HID_USAGE_PID_ET_SAWTOOTH_DOWN:
            UsageText := 'Effect Type Sawtooth Down';
          HID_USAGE_PID_ET_SPRING:
            UsageText := 'Effect Type Spring';
          HID_USAGE_PID_ET_DAMPER:
            UsageText := 'Effect Type Damper';
          HID_USAGE_PID_ET_INERTIA:
            UsageText := 'Effect Type Inertia';
          HID_USAGE_PID_ET_FRICTION:
            UsageText := 'Effect Type Friction';
          HID_USAGE_PID_DURATION:
            UsageText := 'Duration';
          HID_USAGE_PID_SAMPLE_PERIOD:
            UsageText := 'Sample Period';
          HID_USAGE_PID_GAIN:
            UsageText := 'Gain';
          HID_USAGE_PID_TRIGGER_BUTTON:
            UsageText := 'Trigger Button';
          HID_USAGE_PID_TRIGGER_REPEAT_INTERVAL:
            UsageText := 'Trigger Repeat Interval';
          HID_USAGE_PID_AXES_ENABLE:
            UsageText := 'Axes Enable';
          HID_USAGE_PID_DIRECTION_ENABLE:
            UsageText := 'Direction Enable';
          HID_USAGE_PID_DIRECTION:
            UsageText := 'Direction';
          HID_USAGE_PID_TYPE_SPECIFIC_BLOCK_OFFSET:
            UsageText := 'Type Specific Block Offset';
          HID_USAGE_PID_BLOCK_TYPE:
            UsageText := 'Block Type';
          HID_USAGE_PID_SET_ENVELOPE_REPORT:
            UsageText := 'Set Envelope Report';
          HID_USAGE_PID_ATTACK_LEVEL:
            UsageText := 'Attack Level';
          HID_USAGE_PID_ATTACK_TIME:
            UsageText := 'Attack Time';
          HID_USAGE_PID_FADE_LEVEL:
            UsageText := 'Fade Level';
          HID_USAGE_PID_FADE_TIME:
            UsageText := 'Fade Time';
          HID_USAGE_PID_SET_CONDITION_REPORT:
            UsageText := 'Set Condition Report';
          HID_USAGE_PID_CP_OFFSET:
            UsageText := 'CP Offset';
          HID_USAGE_PID_POSITIVE_COEFFICIENT:
            UsageText := 'Positive Coefficient';
          HID_USAGE_PID_NEGATIVE_COEFFICIENT:
            UsageText := 'Negative Coefficient';
          HID_USAGE_PID_POSITIVE_SATURATION:
            UsageText := 'Positive Saturation';
          HID_USAGE_PID_NEGATIVE_SATURATION:
            UsageText := 'Negative Saturation';
          HID_USAGE_PID_DEAD_BAND:
             UsageText := 'Dead Band';
         HID_USAGE_PID_DOWNLOAD_FORCE_SAMPLE:
            UsageText := 'Download Force Sample';
          HID_USAGE_PID_ISOCH_CUSTOM_FORCE_ENABLE:
            UsageText := 'Isoch Custom Force Enable';
          HID_USAGE_PID_CUSTOM_FORCE_DATA_REPORT:
            UsageText := 'Custom Force Data Report';
          HID_USAGE_PID_CUSTOM_FORCE_DATA:
            UsageText := 'Custom Force Data';
          HID_USAGE_PID_CUSTOM_FORCE_VENDOR_DEFINED_DATA:
            UsageText := 'Custom Force Vendor Defined Data';
          HID_USAGE_PID_SET_CUSTOM_FORCE_REPORT:
            UsageText := 'Set Custom Force Report';
          HID_USAGE_PID_CUSTOM_FORCE_DATA_OFFSET:
            UsageText := 'Custom Force Data Report';
          HID_USAGE_PID_SAMPLE_COUNT:
            UsageText := 'Sample Count';
          HID_USAGE_PID_SET_PERIODIC_REPORT:
            UsageText := 'Set Periodic Report';
          HID_USAGE_PID_OFFSET:
            UsageText := 'Offset';
          HID_USAGE_PID_MAGNITUDE:
            UsageText := 'Magnitude';
          HID_USAGE_PID_PHASE:
            UsageText := 'Phase';
          HID_USAGE_PID_PERIOD:
            UsageText := 'Period';
          HID_USAGE_PID_SET_CONSTANT_FORCE_REPORT:
            UsageText := 'Set Constant Force Report';
          HID_USAGE_PID_SET_RAMP_FORCE_REPORT:
            UsageText := 'Set Ramp Force Report';
          HID_USAGE_PID_RAMP_START:
            UsageText := 'Ramp Start';
          HID_USAGE_PID_RAMP_END:
            UsageText := 'Ramp Stop';
          HID_USAGE_PID_EFFECT_OPERATION_REPORT:
            UsageText := 'Effect Operation Report';
          HID_USAGE_PID_EFFECT_OPERATION:
            UsageText := 'Effect Operation';
          HID_USAGE_PID_OP_EFFECT_START:
            UsageText := 'OP Effect Start';
          HID_USAGE_PID_OP_EFFECT_START_SOLO:
            UsageText := 'OP Effect Start Solo';
          HID_USAGE_PID_OP_EFFECT_STOP:
            UsageText := 'OP Effect Stop';
          HID_USAGE_PID_LOOP_COUNT:
            UsageText := 'Loop Count';
          HID_USAGE_PID_DEVICE_GAIN_REPORT:
            UsageText := 'Device Gain Report';
          HID_USAGE_PID_DEVICE_GAIN:
            UsageText := 'Device Gain';
          HID_USAGE_PID_PID_POOL_REPORT:
            UsageText := 'Pool Report';
          HID_USAGE_PID_RAM_POOL_SIZE:
            UsageText := 'RAM Pool Size';
          HID_USAGE_PID_ROM_POOL_SIZE:
            UsageText := 'ROM Pool Size';
          HID_USAGE_PID_ROM_EFFECT_BLOCK_COUNT:
            UsageText := 'Effect Block Count';
          HID_USAGE_PID_SIMULTANEOUS_EFFECTS_MAX:
            UsageText := 'Simultaneous Effects Max';
          HID_USAGE_PID_POOL_ALIGNMENT:
            UsageText := 'Pool Alignment';
          HID_USAGE_PID_PID_POOL_MOVE_REPORT:
            UsageText := 'Pool Move Report';
          HID_USAGE_PID_MOVE_SOURCE:
            UsageText := 'Move Source';
          HID_USAGE_PID_MOVE_DESTINATION:
            UsageText := 'Move Destination';
          HID_USAGE_PID_MOVE_LENGTH:
            UsageText := 'Move Length';
          HID_USAGE_PID_PID_BLOCK_LOAD_REPORT:
            UsageText := 'PID Block Load Report';
          HID_USAGE_PID_BLOCK_LOAD_STATUS:
            UsageText := 'Block Load Status';
          HID_USAGE_PID_BLOCK_LOAD_SUCCESS:
            UsageText := 'Load Success';
          HID_USAGE_PID_BLOCK_LOAD_FULL:
            UsageText := 'Load Full';
          HID_USAGE_PID_BLOCK_LOAD_ERROR:
            UsageText := 'Load Error';
          HID_USAGE_PID_BLOCK_HANDLE:
            UsageText := 'Block Handle';
          HID_USAGE_PID_PID_BLOCK_FREE_REPORT:
            UsageText := 'PID Block Free Report';
          HID_USAGE_PID_TYPE_SPECIFIC_BLOCK_HANDLE:
            UsageText := 'Type Specific Block Handle';
          HID_USAGE_PID_PID_STATE_REPORT:
            UsageText := 'PID State Report';
          HID_USAGE_PID_EFFECT_PLAYING:
            UsageText := 'Effect Playing';
          HID_USAGE_PID_PID_DEVICE_CONTROL_REPORT:
            UsageText := 'PID Device Control Report';
          HID_USAGE_PID_PID_DEVICE_CONTROL:
            UsageText := 'PID Device Control';
          HID_USAGE_PID_DC_ENABLE_ACTUATORS:
            UsageText := 'Device Control Enable Actuators';
          HID_USAGE_PID_DC_DISABLE_ACTUATORS:
            UsageText := 'Device Control Disable Actuators';
          HID_USAGE_PID_DC_STOP_ALL_EFFECTS:
            UsageText := 'Device Control Stop All Effects';
          HID_USAGE_PID_DC_DEVICE_RESET:
            UsageText := 'Device Control Device Reset';
          HID_USAGE_PID_DC_DEVICE_PAUSE:
            UsageText := 'Device Control Device Pause';
          HID_USAGE_PID_DC_DEVICE_CONTINUE:
            UsageText := 'Device Control Device Continue';
          HID_USAGE_PID_DEVICE_PAUSED:
            UsageText := 'Device Paused';
          HID_USAGE_PID_ACTUATORS_ENABLED:
            UsageText := 'Actuators Enabled';
          HID_USAGE_PID_SAFETY_SWITCH:
            UsageText := 'Safety Switch';
          HID_USAGE_PID_ACTUATOR_OVERRIDE_SWITCH:
            UsageText := 'Actuator Override Switch';
          HID_USAGE_PID_ACTUATOR_POWER:
            UsageText := 'Actuator Power';
          HID_USAGE_PID_START_DELAY:
            UsageText := 'Start Delay';
          HID_USAGE_PID_PARAMETER_BLOCK_SIZE:
            UsageText := 'Parameter Block Size';
          HID_USAGE_PID_DEVICE_MANAGED_POOL:
            UsageText := 'Device Managed Pool';
          HID_USAGE_PID_SHARED_PARAMETER_BLOCKS:
            UsageText := 'Shared Parameter Blocks';
          HID_USAGE_PID_CREATE_NEW_EFFECT_REPORT:
            UsageText := 'Create New Effect Report';
          HID_USAGE_PID_RAM_POOL_AVAILABLE:
            UsageText := 'RAM Pool Available';
        end;
      end;
    HID_USAGE_PAGE_UNICODE:
      UsagePageText := 'Unicode';
    HID_USAGE_PAGE_ALPHANUMERIC:
      begin
        UsagePageText := 'Alphanumeric';
        case Usage of
          HID_USAGE_ALNUM_DISPLAY_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_ALNUM_DISPLAY_ALPHANUMERIC_DISPLAY:
            UsageText := 'Alphanumeric Display';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_ATTRIBUTES_REPORT:
            UsageText := 'Display Attributes Report';
          HID_USAGE_ALNUM_DISPLAY_ASCII_CHARSET:
            UsageText := 'ASCII Character Set';
          HID_USAGE_ALNUM_DISPLAY_DATA_READ_BACK:
            UsageText := 'Data Read Back';
          HID_USAGE_ALNUM_DISPLAY_FONT_READ_BACK:
            UsageText := 'Font Read Back';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_CONTROL_REPORT:
            UsageText := 'Display Control Report';
          HID_USAGE_ALNUM_DISPLAY_CLEAR_DISPLAY:
            UsageText := 'Clear Display';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_ENABLE:
            UsageText := 'Display Enable';
          HID_USAGE_ALNUM_DISPLAY_SCREEN_SAVER_DELAY:
            UsageText := 'Screen Saver Delay';
          HID_USAGE_ALNUM_DISPLAY_SCREEN_SAVER_ENABLE:
            UsageText := 'Screen Saver Enable';
          HID_USAGE_ALNUM_DISPLAY_VERTICAL_SCROLL:
            UsageText := 'Vertical Scroll';
          HID_USAGE_ALNUM_DISPLAY_HORIZONTAL_SCROLL:
            UsageText := 'Horizontal Scroll';
          HID_USAGE_ALNUM_DISPLAY_CHARACTER_REPORT:
            UsageText := 'Character Report';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_DATA:
            UsageText := 'Display Data';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_STATUS:
            UsageText := 'Display Status';
          HID_USAGE_ALNUM_DISPLAY_STAT_NOT_READY:
            UsageText := 'Stat Not Ready';
          HID_USAGE_ALNUM_DISPLAY_STAT_READY:
            UsageText := 'Stat Ready';
          HID_USAGE_ALNUM_DISPLAY_ERR_NOT_A_LOADABLE_CHAR:
            UsageText := 'Error Not a Loadable Char';
          HID_USAGE_ALNUM_DISPLAY_ERR_FONT_DATA_CANNOT_BE_READ:
            UsageText := 'Error Font Data Cannot Be Read';
          HID_USAGE_ALNUM_DISPLAY_CURSOR_POSITION_REPORT:
            UsageText := 'Cursor Position Report';
          HID_USAGE_ALNUM_DISPLAY_ROW:
            UsageText := ' Row';
          HID_USAGE_ALNUM_DISPLAY_COLUMN:
            UsageText := 'Column';
          HID_USAGE_ALNUM_DISPLAY_ROWS:
            UsageText := 'Rows';
          HID_USAGE_ALNUM_DISPLAY_COLUMNS:
            UsageText := 'Columns';
          HID_USAGE_ALNUM_DISPLAY_CURSOR_PIXEL_POSITIONING:
            UsageText := 'Cursor Pixel Positioning';
          HID_USAGE_ALNUM_DISPLAY_CURSOR_MODE:
            UsageText := 'Cursor Mode';
          HID_USAGE_ALNUM_DISPLAY_CURSOR_ENABLE:
            UsageText := 'Cursor Enable';
          HID_USAGE_ALNUM_DISPLAY_CURSOR_BLINK:
            UsageText := 'Cursor Blink';
          HID_USAGE_ALNUM_DISPLAY_FONT_REPORT:
            UsageText := 'Font Report';
          HID_USAGE_ALNUM_DISPLAY_FONT_DATA:
            UsageText := 'Font Data';
          HID_USAGE_ALNUM_DISPLAY_CHAR_WIDTH:
            UsageText := 'Character Width';
          HID_USAGE_ALNUM_DISPLAY_CHAR_HEIGHT:
            UsageText := 'Character Height';
          HID_USAGE_ALNUM_DISPLAY_CHAR_SPACING_HORIZONTAL:
            UsageText := 'Character Spacing Horizontal';
          HID_USAGE_ALNUM_DISPLAY_CHAR_SPACING_VERTICAL:
            UsageText := 'Character Spacing Vertical';
          HID_USAGE_ALNUM_DISPLAY_UNICODE_CHARSET:
            UsageText := 'Unicode Character Set';
          HID_USAGE_ALNUM_DISPLAY_FONT_7_SEGMENT:
            UsageText := 'Font 7 Segment';
          HID_USAGE_ALNUM_DISPLAY_7_SEGMENT_DIRECT_MAP:
            UsageText := '7 Segment Direct Map';
          HID_USAGE_ALNUM_DISPLAY_FONT_14_SEGMENT:
            UsageText := 'Font 14 Segment';
          HID_USAGE_ALNUM_DISPLAY_14_SEGMENT_DIRECT_MAP:
            UsageText := '14 Segment Direct Map';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_BRIGHTNESS:
            UsageText := 'Display Brightness';
          HID_USAGE_ALNUM_DISPLAY_DISPLAY_CONTRAST:
            UsageText := 'Display Contrast';
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTRIBUTE:
            UsageText := 'Character Attribute';
          HID_USAGE_ALNUM_DISPLAY_ATTRIBUTE_READBACK:
            UsageText := 'Attribute Read Back';
          HID_USAGE_ALNUM_DISPLAY_ATTRIBUTE_DATA:
            UsageText := 'Attribute Data';
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_ENHANCE:
            UsageText := 'Character Attribute Enhance';
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_UNDERLINE:
            UsageText := 'Character Attribute Underline';
          HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_BLINK:
            UsageText := 'Character Attribute Blink';
        end;
      end;
    HID_USAGE_PAGE_MEDICAL_INSTRUMENT:
      begin
        UsagePageText := 'Medical Instrument';
        case Usage of
          HID_USAGE_MEDICAL_INSTRUMENT_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_MEDICAL_INSTRUMENT_MEDICAL_ULTRASOUND:
            UsageText := 'Medical Ultrasound';
          HID_USAGE_MEDICAL_INSTRUMENT_VCR_AQUISITION:
            UsageText := 'VCR Aquisition';
          HID_USAGE_MEDICAL_INSTRUMENT_FREEZE_THAW:
            UsageText := 'Freeze/Thaw';
          HID_USAGE_MEDICAL_INSTRUMENT_CLIP_STORE:
            UsageText := 'Clip Store';
          HID_USAGE_MEDICAL_INSTRUMENT_UPDATE:
            UsageText := 'Update';
          HID_USAGE_MEDICAL_INSTRUMENT_NEXT:
            UsageText := 'Next';
          HID_USAGE_MEDICAL_INSTRUMENT_SAVE:
            UsageText := 'Save';
          HID_USAGE_MEDICAL_INSTRUMENT_PRINT:
            UsageText := 'Print';
          HID_USAGE_MEDICAL_INSTRUMENT_MICROPHONE_ENABLE:
            UsageText := 'Microphone Enable';
          HID_USAGE_MEDICAL_INSTRUMENT_CINE:
            UsageText := 'Cine';
          HID_USAGE_MEDICAL_INSTRUMENT_TRANSMIT_POWER:
            UsageText := 'Transmit Power';
          HID_USAGE_MEDICAL_INSTRUMENT_VOLUME:
            UsageText := 'Volume';
          HID_USAGE_MEDICAL_INSTRUMENT_FOCUS:
            UsageText := 'Focus';
          HID_USAGE_MEDICAL_INSTRUMENT_DEPTH:
            UsageText := 'Depth';
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_STEP_PRIMARY:
            UsageText := 'Soft Step Primary';
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_STEP_SECONDARY:
            UsageText := 'Soft Step Secondary';
          HID_USAGE_MEDICAL_INSTRUMENT_DEPTH_GAIN_COMPENSATION:
            UsageText := 'Depth Gain Compensation';
          HID_USAGE_MEDICAL_INSTRUMENT_ZOOM_SELECT:
            UsageText := 'Zoom Select';
          HID_USAGE_MEDICAL_INSTRUMENT_ZOOM_ADJUST:
            UsageText := 'Zoom Adjust';
          HID_USAGE_MEDICAL_INSTRUMENT_SPECTRAL_DOPPLER_MODE_SELECT:
            UsageText := 'Spectral Doppler Mode Select';
          HID_USAGE_MEDICAL_INSTRUMENT_SPECTRAL_DOPPLER_ADJUST:
            UsageText := 'Spectral Doppler Adjust';
          HID_USAGE_MEDICAL_INSTRUMENT_COLOR_DOPPLER_MODE_SELECT:
            UsageText := 'Color Doppler Mode Select';
          HID_USAGE_MEDICAL_INSTRUMENT_COLOR_DOPPLER_ADJUST:
            UsageText := 'Color Doppler Adjust';
          HID_USAGE_MEDICAL_INSTRUMENT_MOTION_MODE_SELECT:
            UsageText := 'Motion Mode Select';
          HID_USAGE_MEDICAL_INSTRUMENT_MOTION_MODE_ADJUST:
            UsageText := 'Motion Mode Adjust';
          HID_USAGE_MEDICAL_INSTRUMENT_2D_MODE_SELECT:
            UsageText := '2D Mode Select';
          HID_USAGE_MEDICAL_INSTRUMENT_2D_MODE_ADJUST:
            UsageText := '2D Mode Adjust';
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_CONTROL_SELECT:
            UsageText := 'Soft Control Select';
          HID_USAGE_MEDICAL_INSTRUMENT_SOFT_CONTROL_ADJUST:
            UsageText := 'Soft Control Adjust';
        end;
      end;
    HID_USAGE_PAGE_USB_MONITOR:
      begin
        UsagePageText := 'USB Monitor';
        case Usage of
          HID_USAGE_MONITOR_RESERVED:
            UsageText := 'Reserved';
          HID_USAGE_MONITOR_MONITOR_CONTROL:
            UsageText := 'Monitor Control';
          HID_USAGE_MONITOR_EDID_INFORMATION:
            UsageText := 'EDID Information';
          HID_USAGE_MONITOR_VDIF_INFORMATION:
            UsageText := 'VDIF Information';
          HID_USAGE_MONITOR_VESA_VERSION:
            UsageText := 'VESA Version';
        end;
      end;
    HID_USAGE_PAGE_MONITOR_ENUMERATED_VALUES:
      begin
        UsagePageText := 'USB Enumerated Values';
        case Usage of
          HID_USAGE_MONITOR_ENUM_VALUE_NO_VALUE:
            UsageText := 'No Enumerated Value Selected';
        end;
      end;
    HID_USAGE_PAGE_VESA_VIRTUAL_CONTROLS:
      UsagePageText := 'VESA Virtual Controls';
    HID_USAGE_PAGE_RESERVED:
      UsagePageText := 'Reserved';
    HID_USAGE_PAGE_POWER_DEVICE:
      begin
        UsagePageText := 'Power Device';
        case Usage of
          HID_USAGE_POWER_DEVICE_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_POWER_DEVICE_INAME:
            UsageText := 'iName';
          HID_USAGE_POWER_DEVICE_PRESENT_STATUS:
            UsageText := 'Present Status';
          HID_USAGE_POWER_DEVICE_CHANGED_STATUS:
            UsageText := 'Changed Status';
          HID_USAGE_POWER_DEVICE_UPS:
            UsageText := 'UPS';
          HID_USAGE_POWER_DEVICE_POWER_SUPPLY:
            UsageText := 'Power Supply';
          HID_USAGE_POWER_DEVICE_BATTERY_SYSTEM:
            UsageText := 'Battery System';
          HID_USAGE_POWER_DEVICE_BATTERY_SYSTEM_ID:
            UsageText := 'Battery System ID';
          HID_USAGE_POWER_DEVICE_BATTERY:
            UsageText := 'Battery';
          HID_USAGE_POWER_DEVICE_BATTERY_ID:
            UsageText := 'Battery ID';
          HID_USAGE_POWER_DEVICE_CHARGER:
            UsageText := 'Charger';
          HID_USAGE_POWER_DEVICE_CHARGER_ID:
            UsageText := 'Charger ID';
          HID_USAGE_POWER_DEVICE_POWER_CONVERTER:
            UsageText := 'Power Converter';
          HID_USAGE_POWER_DEVICE_POWER_CONVERTER_ID:
            UsageText := 'Power Converter ID';
          HID_USAGE_POWER_DEVICE_OUTLET_SYSTEM:
            UsageText := 'Outlet System';
          HID_USAGE_POWER_DEVICE_OUTLET_SYSTEM_ID:
            UsageText := 'Outlet System ID';
          HID_USAGE_POWER_DEVICE_INPUT:
            UsageText := 'Input';
          HID_USAGE_POWER_DEVICE_INPUT_ID:
            UsageText := 'Input ID';
          HID_USAGE_POWER_DEVICE_OUTPUT:
            UsageText := 'Output';
          HID_USAGE_POWER_DEVICE_OUTPUT_ID:
            UsageText := 'Output ID';
          HID_USAGE_POWER_DEVICE_FLOW:
            UsageText := 'Flow';
          HID_USAGE_POWER_DEVICE_FLOW_ID:
            UsageText := 'Flow ID';
          HID_USAGE_POWER_DEVICE_OUTLET:
            UsageText := 'Outlet';
          HID_USAGE_POWER_DEVICE_OUTLET_ID:
            UsageText := 'Outlet ID';
          HID_USAGE_POWER_DEVICE_GANG:
            UsageText := 'Gang';
          HID_USAGE_POWER_DEVICE_GANG_ID:
            UsageText := 'Gang ID';
          HID_USAGE_POWER_DEVICE_POWER_SUMMARY:
            UsageText := 'Power Summary';
          HID_USAGE_POWER_DEVICE_POWER_SUMMARY_ID:
            UsageText := 'Power Summary ID';
          HID_USAGE_POWER_DEVICE_VOLTAGE:
            UsageText := 'Voltage';
          HID_USAGE_POWER_DEVICE_CURRENT:
            UsageText := 'Current';
          HID_USAGE_POWER_DEVICE_FREQUENCY:
            UsageText := 'Frequency';
          HID_USAGE_POWER_DEVICE_APPARENT_POWER:
            UsageText := 'Apparent Power';
          HID_USAGE_POWER_DEVICE_ACTIVE_POWER:
            UsageText := 'Active Power';
          HID_USAGE_POWER_DEVICE_PERCENT_LOAD:
            UsageText := 'Percent Load';
          HID_USAGE_POWER_DEVICE_TEMPERATURE:
            UsageText := 'Temperature';
          HID_USAGE_POWER_DEVICE_HUMIDITY:
            UsageText := 'Humidity';
          HID_USAGE_POWER_DEVICE_BAD_COUNT:
            UsageText := 'Bad Count';
          HID_USAGE_POWER_DEVICE_CONFIG_VOLTAGE:
            UsageText := 'Config Voltage';
          HID_USAGE_POWER_DEVICE_CONFIG_CURRENT:
            UsageText := 'Config Current';
          HID_USAGE_POWER_DEVICE_CONFIG_FREQUENCY:
            UsageText := 'Config Frequency';
          HID_USAGE_POWER_DEVICE_CONFIG_APPARENT_POWER:
            UsageText := 'Config Apparent Power';
          HID_USAGE_POWER_DEVICE_CONFIG_ACTIVE_POWER:
            UsageText := 'Config Active Power';
          HID_USAGE_POWER_DEVICE_CONFIG_PERCENT_LOAD:
            UsageText := 'Config Percent Load';
          HID_USAGE_POWER_DEVICE_CONFIG_TEMPERATURE:
            UsageText := 'Config Temperature';
          HID_USAGE_POWER_DEVICE_CONFIG_HUMIDITY:
            UsageText := 'Config Humidity';
          HID_USAGE_POWER_DEVICE_SWITCH_ON_CONTROL:
            UsageText := 'Switch On Control';
          HID_USAGE_POWER_DEVICE_SWITCH_OFF_CONTROL:
            UsageText := 'Switch Off Control';
          HID_USAGE_POWER_DEVICE_TOGGLE_CONTROL:
            UsageText := 'Toggle Control';
          HID_USAGE_POWER_DEVICE_LOW_VOLTAGE_TRANSFER:
            UsageText := 'Low Voltage Transfer';
          HID_USAGE_POWER_DEVICE_HIGH_VOLTAGE_TRANSFER:
            UsageText := 'High Voltage Transfer';
          HID_USAGE_POWER_DEVICE_DELAY_BEFORE_REBOOT:
            UsageText := 'Delay Before Reboot';
          HID_USAGE_POWER_DEVICE_DELAY_BEFORE_STARTUP:
            UsageText := 'Delay Before Startup';
          HID_USAGE_POWER_DEVICE_DELAY_BEFORE_SHUTDOWN:
            UsageText := 'Delay Before Shutdown';
          HID_USAGE_POWER_DEVICE_TEST:
            UsageText := 'Test';
          HID_USAGE_POWER_DEVICE_MODULE_RESET:
            UsageText := 'Module Reset';
          HID_USAGE_POWER_DEVICE_AUDIBLE_ALARM_CONTROL:
            UsageText := 'Audible Alarm Control';
          HID_USAGE_POWER_DEVICE_PRESENT:
            UsageText := 'Present';
          HID_USAGE_POWER_DEVICE_GOOD:
            UsageText := 'Good';
          HID_USAGE_POWER_DEVICE_INTERNAL_FAILURE:
            UsageText := 'Internal Failure';
          HID_USAGE_POWER_DEVICE_VOLTAGE_OUT_OF_RANGE:
            UsageText := 'Voltage Out of Range';
          HID_USAGE_POWER_DEVICE_FREQUENCY_OUT_OF_RANGE:
            UsageText := 'Frequency Out of Range';
          HID_USAGE_POWER_DEVICE_OVERLOAD:
            UsageText := 'Overload';
          HID_USAGE_POWER_DEVICE_OVERCHARGED:
            UsageText := 'Overcharged';
          HID_USAGE_POWER_DEVICE_OVERTEMPERATURE:
            UsageText := 'Overtemperature';
          HID_USAGE_POWER_DEVICE_SHUTDOWN_REQUESTED:
            UsageText := 'Shutdown Requested';
          HID_USAGE_POWER_DEVICE_SHUTDOWN_IMMINENT:
            UsageText := 'Shutdown Imminent';
          HID_USAGE_POWER_DEVICE_SWITCH_ON_OFF:
            UsageText := 'Switch On/Off';
          HID_USAGE_POWER_DEVICE_SWITCHABLE:
            UsageText := 'Switchable';
          HID_USAGE_POWER_DEVICE_USED:
            UsageText := 'Used';
          HID_USAGE_POWER_DEVICE_BOOST:
            UsageText := 'Boost';
          HID_USAGE_POWER_DEVICE_BUCK:
            UsageText := 'Buck';
          HID_USAGE_POWER_DEVICE_INITIALIZED:
            UsageText := 'Initialized';
          HID_USAGE_POWER_DEVICE_TESTED:
            UsageText := 'Tested';
          HID_USAGE_POWER_DEVICE_AWAITING_POWER:
            UsageText := 'Awaiting Power';
          HID_USAGE_POWER_DEVICE_COMMUNICATION_LOST:
            UsageText := 'Communication Lost';
          HID_USAGE_POWER_DEVICE_IMANUFACTURER:
            UsageText := 'iManufacturer';
          HID_USAGE_POWER_DEVICE_IPRODUCT:
            UsageText := 'iProduct';
          HID_USAGE_POWER_DEVICE_ISERIALNUMBER:
            UsageText := 'iSerialNumber';
        end;
      end;
    HID_USAGE_PAGE_BATTERY_SYSTEM:
      begin
        UsagePageText := 'Battery System';
        case Usage of
          HID_USAGE_BATTERY_SYSTEM_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_BATTERY_SYSTEM_SMB_BATTERY_MODE:
            UsageText := 'SMB Battery Mode';
          HID_USAGE_BATTERY_SYSTEM_SMB_BATTERY_STATUS:
            UsageText := 'SMB Battery Status';
          HID_USAGE_BATTERY_SYSTEM_SMB_ALARM_WARNING:
            UsageText := 'SMB Alarm Warning';
          HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_MODE:
            UsageText := 'SMB Charger Mode';
          HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_STATUS:
            UsageText := 'SMB Charger Status';
          HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_SPEC_INFO:
            UsageText := 'SMB Charger Spec Info';
          HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_STATE:
            UsageText := 'SMB Selector state';
          HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_PRESETS:
            UsageText := 'SMB Selector Presets';
          HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_INFO:
            UsageText := 'SMB Selector Info';
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_1:
            UsageText := 'Optional Manufacturer Function 1';
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_2:
            UsageText := 'Optional Manufacturer Function 2';
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_3:
            UsageText := 'Optional Manufacturer Function 3';
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_4:
            UsageText := 'Optional Manufacturer Function 4';
          HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_5:
            UsageText := 'Optional Manufacturer Function 5';
          HID_USAGE_BATTERY_SYSTEM_CONNECTION_TO_SMBUS:
            UsageText := 'Connection to SMBus';
          HID_USAGE_BATTERY_SYSTEM_OUTPUT_CONNECTION:
            UsageText := 'Output Connection';
          HID_USAGE_BATTERY_SYSTEM_CHARGER_CONNECTION:
            UsageText := 'Charger Connection';
          HID_USAGE_BATTERY_SYSTEM_BATTERY_INSERTION:
            UsageText := 'Battery Insertion';
          HID_USAGE_BATTERY_SYSTEM_USE_NEXT:
            UsageText := 'Use Next';
          HID_USAGE_BATTERY_SYSTEM_OK_TO_USE:
            UsageText := 'OK to Use';
          HID_USAGE_BATTERY_SYSTEM_BATTERY_SUPPORTED:
            UsageText := 'Battery Supported';
          HID_USAGE_BATTERY_SYSTEM_SELECTOR_REVISION:
            UsageText := 'Selector Revision';
          HID_USAGE_BATTERY_SYSTEM_CHARGING_INDICATOR:
            UsageText := 'Charging Indicator';
          HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_ACCESS:
            UsageText := 'Manufacturer Access';
          HID_USAGE_BATTERY_SYSTEM_REMAINING_CAPACITY_LIMIT:
            UsageText := 'Remaining Capacity Limit';
          HID_USAGE_BATTERY_SYSTEM_REMAINING_TIME_LIMIT:
            UsageText := 'Remaining Time Limit';
          HID_USAGE_BATTERY_SYSTEM_AT_RATE:
            UsageText := 'At Rate';
          HID_USAGE_BATTERY_SYSTEM_CAPACITY_MODE:
            UsageText := 'Capacity Mode';
          HID_USAGE_BATTERY_SYSTEM_BROADCAST_TO_CHARGER:
            UsageText := 'Broadcast to Charger';
          HID_USAGE_BATTERY_SYSTEM_PRIMARY_BATTERY:
            UsageText := 'Primary Battery';
          HID_USAGE_BATTERY_SYSTEM_CHARGE_CONTROLLER:
            UsageText := 'Charge Controller';
          HID_USAGE_BATTERY_SYSTEM_TERMINATE_CHARGE:
            UsageText := 'Terminate Charge';
          HID_USAGE_BATTERY_SYSTEM_TERMINATE_DISCHARGE:
            UsageText := 'Terminate Discharge';
          HID_USAGE_BATTERY_SYSTEM_BELOW_REMAINING_CAPACITY_LIMIT:
            UsageText := 'Below Remaining Capacity';
          HID_USAGE_BATTERY_SYSTEM_REMAINING_TIME_LIMIT_EXPIRED:
            UsageText := 'Remaining Time Limit Expired';
          HID_USAGE_BATTERY_SYSTEM_CHARGING:
            UsageText := 'Charging';
          HID_USAGE_BATTERY_SYSTEM_DISCHARGING:
            UsageText := 'Discharging';
          HID_USAGE_BATTERY_SYSTEM_FULLY_CHARGED:
            UsageText := 'Fully Charged';
          HID_USAGE_BATTERY_SYSTEM_FULLY_DISCHARGED:
            UsageText := 'Fully Discharged';
          HID_USAGE_BATTERY_SYSTEM_CONDITIONING_FLAG:
            UsageText := 'Conditioning Flag';
          HID_USAGE_BATTERY_SYSTEM_AT_RATE_OK:
            UsageText := 'At Rate OK';
          HID_USAGE_BATTERY_SYSTEM_SMB_ERROR_CODE:
            UsageText := 'SMB Error Code';
          HID_USAGE_BATTERY_SYSTEM_NEED_REPLACEMENT:
            UsageText := 'Need Replacement';
          HID_USAGE_BATTERY_SYSTEM_AT_RATE_TIME_TO_FULL:
            UsageText := 'At Rate Time to Full';
          HID_USAGE_BATTERY_SYSTEM_AT_RATE_TIME_TO_EMPTY:
            UsageText := 'At Rate Time to Empty';
          HID_USAGE_BATTERY_SYSTEM_AVERAGE_CURRENT:
            UsageText := 'Average Current';
          HID_USAGE_BATTERY_SYSTEM_MAX_ERROR:
            UsageText := 'Max Error';
          HID_USAGE_BATTERY_SYSTEM_RELATIVE_STATE_OF_CHARGE:
            UsageText := 'Relative State of Charge';
          HID_USAGE_BATTERY_SYSTEM_ABSOLUTE_STATE_OF_CHARGE:
            UsageText := 'absolute State of charge';
          HID_USAGE_BATTERY_SYSTEM_REMAINING_CAPACITY:
            UsageText := 'Remaining Capacity';
          HID_USAGE_BATTERY_SYSTEM_FULL_CHARGE_CAPACITY:
            UsageText := 'Full Charge Capacity';
          HID_USAGE_BATTERY_SYSTEM_RUN_TIME_TO_EMPTY:
            UsageText := 'Run Time to Empty';
          HID_USAGE_BATTERY_SYSTEM_AVERAGE_TIME_TO_EMPTY:
            UsageText := 'Average Time to Empty';
          HID_USAGE_BATTERY_SYSTEM_AVERAGE_TIME_TO_FULL:
            UsageText := 'Average Time to Full';
          HID_USAGE_BATTERY_SYSTEM_CYCLE_COUNT:
            UsageText := 'Cycle Count';
          HID_USAGE_BATTERY_SYSTEM_BATT_PACK_MODEL_LEVEL:
            UsageText := 'Battery Pack Model Level';
          HID_USAGE_BATTERY_SYSTEM_INTERNAL_CHARGE_CONTROLLER:
            UsageText := 'Internal Charge Controller';
          HID_USAGE_BATTERY_SYSTEM_PRIMARY_BATTERY_SUPPORT:
            UsageText := 'Primary Battery Support';
          HID_USAGE_BATTERY_SYSTEM_DESIGN_CAPACITY:
            UsageText := 'Design capacity';
          HID_USAGE_BATTERY_SYSTEM_SPECIFICATION_INFO:
            UsageText := 'Specification Info';
          HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_DATE:
            UsageText := 'Manufacturer Date';
          HID_USAGE_BATTERY_SYSTEM_SERIAL_NUMBER:
            UsageText := 'Serial Number';
          HID_USAGE_BATTERY_SYSTEM_I_MANUFACTURER_NAME:
            UsageText := 'iManufacturerName';
          HID_USAGE_BATTERY_SYSTEM_I_DEVICE_NAME:
            UsageText := 'iDeviceName';
          HID_USAGE_BATTERY_SYSTEM_I_DEVICE_CHEMISTERY:
            UsageText := 'iDeviceChemistery';
          HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_DATA:
            UsageText := 'Manufacturer Data';
          HID_USAGE_BATTERY_SYSTEM_RECHARGABLE:
            UsageText := 'Rechargable';
          HID_USAGE_BATTERY_SYSTEM_WARNING_CAPACITY_LIMIT:
            UsageText := 'Warning Capacity Limit';
          HID_USAGE_BATTERY_SYSTEM_CAPACITY_GRANULARITY_1:
            UsageText := 'Capacity Granularity 1';
          HID_USAGE_BATTERY_SYSTEM_CAPACITY_GRANULARITY_2:
            UsageText := 'Capacity Granularity 2';
          HID_USAGE_BATTERY_SYSTEM_I_OEM_INFORMATION:
            UsageText := 'iOEMInformation';
          HID_USAGE_BATTERY_SYSTEM_INHIBIT_CHARGE:
            UsageText := 'Inhibit Charge';
          HID_USAGE_BATTERY_SYSTEM_ENABLE_POLLING:
            UsageText := 'Enable Polling';
          HID_USAGE_BATTERY_SYSTEM_RESET_TO_ZERO:
            UsageText := 'Reset to zero';
          HID_USAGE_BATTERY_SYSTEM_AC_PRESENT:
            UsageText := 'AC Present';
          HID_USAGE_BATTERY_SYSTEM_BATTERY_PRESENT:
            UsageText := 'Battery Present';
          HID_USAGE_BATTERY_SYSTEM_POWER_FAIL:
            UsageText := 'Power Fail';
          HID_USAGE_BATTERY_SYSTEM_ALARM_INHIBITED:
            UsageText := 'Alarm Inhibited';
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_UNDER_RANGE:
            UsageText := 'Thermistor Under range';
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_HOT:
            UsageText := 'Thermistor Hot';
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_COLD:
            UsageText := 'Thermistor Cold';
          HID_USAGE_BATTERY_SYSTEM_THERMISTOR_OVER_RANGE:
            UsageText := 'Thermistor Over Range';
          HID_USAGE_BATTERY_SYSTEM_VOLTAGE_OUT_OF_RANGE:
            UsageText := 'Voltage out of Range';
          HID_USAGE_BATTERY_SYSTEM_CURRENT_OUT_OF_RANGE:
            UsageText := 'Current Out of Range';
          HID_USAGE_BATTERY_SYSTEM_CURRENT_NOT_REGULATED:
            UsageText := 'Current Not Regulated';
          HID_USAGE_BATTERY_SYSTEM_VOLTAGE_NOT_REGULATED:
            UsageText := 'Voltage Not Regulated';
          HID_USAGE_BATTERY_SYSTEM_MASTER_MODE:
            UsageText := 'Master Mode';
          HID_USAGE_BATTERY_SYSTEM_CHARGER_SELECTOR_SUPPORT:
            UsageText := 'Charger Selector Support';
          HID_USAGE_BATTERY_SYSTEM_CHARGER_SPEC:
            UsageText := 'Charger Spec';
          HID_USAGE_BATTERY_SYSTEM_LEVEL_2:
            UsageText := 'Level 2';
          HID_USAGE_BATTERY_SYSTEM_LEVEL_3:
            UsageText := 'Level 3';
        end;
      end;
    HID_USAGE_PAGE_BARCODE_SCANNER:
      begin
        UsagePageText := 'Barcode Scanner';
        case Usage of
          HID_USAGE_BARCODE_SCANNER_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_BADGE_READER:
            UsageText := 'Bar Code Badge Reader';
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_SCANNER:
            UsageText := 'Bar Code Scanner';
          HID_USAGE_BARCODE_SCANNER_DUMB_BAR_CODE_SCANNER:
            UsageText := 'Dumb Bar Code Scanner';
          HID_USAGE_BARCODE_SCANNER_CORDLESS_SCANNER_BASE:
            UsageText := 'Cordless Scanner Base';
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_SCANNER_CRADLE:
            UsageText := 'Bar Code Scanner Cradle';
          HID_USAGE_BARCODE_SCANNER_ATTRIBUTE_REPORT:
            UsageText := 'Attribute Report';
          HID_USAGE_BARCODE_SCANNER_SETTINGS_REPORT:
            UsageText := 'Settings Report';
          HID_USAGE_BARCODE_SCANNER_SCANNED_DATA_REPORT:
            UsageText := 'Scanned Data Report';
          HID_USAGE_BARCODE_SCANNER_RAW_SCANNED_DATA_REPORT:
            UsageText := 'Raw Scanned Data Report';
          HID_USAGE_BARCODE_SCANNER_TRIGGER_REPORT:
            UsageText := 'Trigger Report';
          HID_USAGE_BARCODE_SCANNER_STATUS_REPORT:
            UsageText := 'Status Report';
          HID_USAGE_BARCODE_SCANNER_UPC_EAN_CONTROL_REPORT:
            UsageText := 'UPC/EAN Control Report';
          HID_USAGE_BARCODE_SCANNER_EAN_2_3_LABEL_CONTROL_REPORT:
            UsageText := 'EAN 2/3 Label Control Report';
          HID_USAGE_BARCODE_SCANNER_CODE_39_CONTROL_REPORT:
            UsageText := 'Code 39 Control Report';
          HID_USAGE_BARCODE_SCANNER_INTERLEAVED_2_OF_5_CONTROL_REPORT:
            UsageText := 'Interleaved 2 of 5 Control Report';
          HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5_CONTROL_REPORT:
            UsageText := 'Standard 2 of 5 Control Report';
          HID_USAGE_BARCODE_SCANNER_MSI_PLESSEY_CONTROL_REPORT:
            UsageText := 'MSI Plessey Control Report';
          HID_USAGE_BARCODE_SCANNER_CODABAR_CONTROL_REPORT:
            UsageText := 'Codabar Control Report';
          HID_USAGE_BARCODE_SCANNER_CODE_128_CONTROL_REPORT:
            UsageText := 'Code 128 Control Report';
          HID_USAGE_BARCODE_SCANNER_MISC_1D_CONTROL_REPORT:
            UsageText := 'Misc 1D Control Report';
          HID_USAGE_BARCODE_SCANNER_2D_CONTROL_REPORT:
            UsageText := '2D Control Report';
          HID_USAGE_BARCODE_SCANNER_AIMING_POINTER_MODE:
            UsageText := 'Aiming/Pointer Mode';
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_PRESENT_SENSOR:
            UsageText := 'Bar Code Present Sensor';
          HID_USAGE_BARCODE_SCANNER_CLASS_1A_LASER:
            UsageText := 'Class 1A Laser';
          HID_USAGE_BARCODE_SCANNER_CLASS_2_LASER:
            UsageText := 'Class 2 Laser';
          HID_USAGE_BARCODE_SCANNER_HEATER_PRESENT:
            UsageText := 'Heater Present';
          HID_USAGE_BARCODE_SCANNER_CONTACT_SCANNER:
            UsageText := 'Contact Scanner';
          HID_USAGE_BARCODE_SCANNER_ELECTRONIC_ARTICLE_SURVEILLANCE_NOTIFICATION:
            UsageText := 'Electronic Article Surveillance Notification';
          HID_USAGE_BARCODE_SCANNER_CONSTANT_ARTICLE_SURVEILLANCE_NOTIFICATION:
            UsageText := 'Constant Article Surveillance Notification';
          HID_USAGE_BARCODE_SCANNER_ERROR_INDICATION:
            UsageText := 'Error Indication';
          HID_USAGE_BARCODE_SCANNER_FIXED_BEEPER:
            UsageText := 'Fixed Beeper';
          HID_USAGE_BARCODE_SCANNER_GOOD_DECODE_INDICATION:
            UsageText := 'Good Decode Indication';
          HID_USAGE_BARCODE_SCANNER_HANDS_FREE_SCANNING:
            UsageText := 'Hands Free Scanning';
          HID_USAGE_BARCODE_SCANNER_INTRINSICALLY_SAFE:
            UsageText := 'Intrinsically Safe';
          HID_USAGE_BARCODE_SCANNER_KLASSE_EINS_LASER:
            UsageText := 'Klasse Eins Laser (Class 1 Laser)';
          HID_USAGE_BARCODE_SCANNER_LONG_RANGE_SCANNER:
            UsageText := 'Long Range Scanner';
          HID_USAGE_BARCODE_SCANNER_MIRROR_SPEED_CONTROL:
            UsageText := 'Mirror Speed Control';
          HID_USAGE_BARCODE_SCANNER_NOT_ON_FILE_INDICATION:
            UsageText := 'Not On File Indication';
          HID_USAGE_BARCODE_SCANNER_PROGRAMMABLE_BEEPER:
            UsageText := 'Programmable Beeper';
          HID_USAGE_BARCODE_SCANNER_TRIGGERLESS:
            UsageText := 'Triggerless';
          HID_USAGE_BARCODE_SCANNER_WAND:
            UsageText := 'Wand';
          HID_USAGE_BARCODE_SCANNER_WATER_RESISTANT:
            UsageText := 'Water Resistant';
          HID_USAGE_BARCODE_SCANNER_MULTI_RANGE_SCANNER:
            UsageText := 'Multi-Range Scanner';
          HID_USAGE_BARCODE_SCANNER_PROXIMITIY_SENSOR:
            UsageText := 'Proximity Sensor';
          HID_USAGE_BARCODE_SCANNER_FRAGMENT_DECODING:
            UsageText := 'Fragment Decoding';
          HID_USAGE_BARCODE_SCANNER_SCANNER_READ_CONFIDENCE:
            UsageText := 'Scanner Read Confidence';
          HID_USAGE_BARCODE_SCANNER_DATA_PREFIX:
            UsageText := 'Data Prefix';
          HID_USAGE_BARCODE_SCANNER_PREFIX_AIMI:
            UsageText := 'Prefix AIMI';
          HID_USAGE_BARCODE_SCANNER_PREFIX_NODE:
            UsageText := 'Prefix Node';
          HID_USAGE_BARCODE_SCANNER_PREFIX_PROPRIETARY:
            UsageText := 'Prefix Proprietary';
          HID_USAGE_BARCODE_SCANNER_ACTIVE_TIME:
            UsageText := 'Active Time';
          HID_USAGE_BARCODE_SCANNER_AIMING_LASER_PATTERN:
            UsageText := 'Aiming Laser Pattern';
          HID_USAGE_BARCODE_SCANNER_BAR_CODE_PRESENT:
            UsageText := 'Bar Code Present';
          HID_USAGE_BARCODE_SCANNER_BEEPER_STATE:
            UsageText := 'Beeper State';
          HID_USAGE_BARCODE_SCANNER_LASER_ON_TIME:
            UsageText := 'Laser On Time';
          HID_USAGE_BARCODE_SCANNER_LASER_STATE:
            UsageText := 'Laser State';
          HID_USAGE_BARCODE_SCANNER_LOCKOUT_TIME:
            UsageText := 'Lockout Time';
          HID_USAGE_BARCODE_SCANNER_MOTOR_STATE:
            UsageText := 'Motor State';
          HID_USAGE_BARCODE_SCANNER_MOTOR_TIMEOUT:
            UsageText := 'Motor Timeout';
          HID_USAGE_BARCODE_SCANNER_POWER_ON_RESET_SCANNER:
            UsageText := 'Power On Reset Scanner';
          HID_USAGE_BARCODE_SCANNER_PREVENT_READ_OF_BARCODES:
            UsageText := 'Prevent Read of Barcodes';
          HID_USAGE_BARCODE_SCANNER_INITIATE_BARCODE_READ:
            UsageText := 'Initiate Barcode Read';
          HID_USAGE_BARCODE_SCANNER_TRIGGER_STATE:
            UsageText := 'Trigger State';
          HID_USAGE_BARCODE_SCANNER_TRIGGER_MODE:
            UsageText := 'Trigger Mode';
          HID_USAGE_BARCODE_SCANNER_TM_BLINKING_LASER_ON:
            UsageText := 'Trigger Mode Blinking Laser On';
          HID_USAGE_BARCODE_SCANNER_TM_CONTINUOUS_LASER_ON:
            UsageText := 'Trigger Mode Continuous Laser On';
          HID_USAGE_BARCODE_SCANNER_TM_LASER_ON_WHILE_PULLED:
            UsageText := 'Trigger Mode Laser On While Pulled';
          HID_USAGE_BARCODE_SCANNER_TM_LASER_STAYS_ON_AFTER_TRIGGER_RELEASE:
            UsageText := 'Trigger Mode Laser Stays On After Trigger Release';
          HID_USAGE_BARCODE_SCANNER_COMMIT_PARAMETERS_TO_NVM:
            UsageText := 'Commit Parameters to NVM';
          HID_USAGE_BARCODE_SCANNER_PARAMETER_SCANNING:
            UsageText := 'Parameter Scanning';
          HID_USAGE_BARCODE_SCANNER_PARAMETERS_CHANGED:
            UsageText := 'Parameters Changed';
          HID_USAGE_BARCODE_SCANNER_SET_PARAMETER_DEFAULT_VALUES:
            UsageText := 'Set Parameter Default Values';
          HID_USAGE_BARCODE_SCANNER_SCANNER_IN_CRADLE:
            UsageText := 'Scanner in Cradle';
          HID_USAGE_BARCODE_SCANNER_SCANNER_IN_RANGE:
            UsageText := 'Scanner in Range';
          HID_USAGE_BARCODE_SCANNER_AIM_DURATION:
            UsageText := 'Aim Duration';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_LAMP_DURATION:
            UsageText := 'Good Read Lamp Duration';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_LAMP_INTENSITY:
            UsageText := 'Good Read Lamp Intensity';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_LED:
            UsageText := 'Good Read LED';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_FREQUENCY:
            UsageText := 'Good Read Tone Frequency';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_LENGTH:
            UsageText := 'Good Read Tone Length';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_VOLUME:
            UsageText := 'Good Read Tone Volume';
          HID_USAGE_BARCODE_SCANNER_NO_READ_MESSAGE:
            UsageText := 'No Read Message';
          HID_USAGE_BARCODE_SCANNER_NOT_ON_FILE_VOLUME:
            UsageText := 'Not On File Volume';
          HID_USAGE_BARCODE_SCANNER_POWERUP_BEEP:
            UsageText := 'Powerup Beep';
          HID_USAGE_BARCODE_SCANNER_SOUND_ERROR_BEEP:
            UsageText := 'Sound Error Beep';
          HID_USAGE_BARCODE_SCANNER_SOUND_GOOD_READ_BEEP:
            UsageText := 'Sound Good Read Beep';
          HID_USAGE_BARCODE_SCANNER_SOUND_NOT_ON_FILE_BEEP:
            UsageText := 'Sound Not On File Beep';
          HID_USAGE_BARCODE_SCANNER_GOOD_READ_WHEN_TO_WRITE:
            UsageText := 'Good Read When to Write';
          HID_USAGE_BARCODE_SCANNER_GRWTI_AFTER_DECODE:
            UsageText := 'GRWTI After Decode';
          HID_USAGE_BARCODE_SCANNER_GRWTI_BEEP_LAMP_AFTER_TRANSMIT:
            UsageText := 'GRWTI Beep/Lamp After Transmit';
          HID_USAGE_BARCODE_SCANNER_GRWTI_NO_BEEP_LAMP_USE_AT_ALL:
            UsageText := 'GRWTI No Beep/Lamp Use at All';
          HID_USAGE_BARCODE_SCANNER_BOOKLAND_EAN:
            UsageText := 'Bookland EAN';
          HID_USAGE_BARCODE_SCANNER_CONVERT_EAN_8_TO_13_TYPE:
            UsageText := 'Convert EAN 8 to 13 Type';
          HID_USAGE_BARCODE_SCANNER_CONVERT_UPC_A_TO_EAN_13:
            UsageText := 'Convert UPC A to EAN-13';
          HID_USAGE_BARCODE_SCANNER_CONVERT_UPC_E_TO_A:
            UsageText := 'Convert UPC-E to A';
          HID_USAGE_BARCODE_SCANNER_EAN_13:
            UsageText := 'EAN-13';
          HID_USAGE_BARCODE_SCANNER_EAN_8:
            UsageText := 'EAN-8';
          HID_USAGE_BARCODE_SCANNER_EAN_99_128_MANDATORY:
            UsageText := 'EAN-99 128 Mandatory';
          HID_USAGE_BARCODE_SCANNER_EAN_99_P5_128_OPTIONAL:
            UsageText := 'EAN-99 P5/128 Optional';
          HID_USAGE_BARCODE_SCANNER_UPC_EAN:
            UsageText := 'UPC/EAN';
          HID_USAGE_BARCODE_SCANNER_UPC_EAN_COUPON_CODE:
            UsageText := 'UPC/EAN Coupon Code';
          HID_USAGE_BARCODE_SCANNER_UPC_EAN_PERIODICALS:
            UsageText := 'UPC/EAN Periodicals';
          HID_USAGE_BARCODE_SCANNER_UPC_A:
            UsageText := 'UPC-A';
          HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_128_MANDATORY:
            UsageText := 'UPC-A with 128 Mandatory';
          HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_128_OPTIONAL:
            UsageText := 'UPC-A with 128 Optional';
          HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_P5_OPTIONAL:
            UsageText := 'UPC-A with P5 Optional';
          HID_USAGE_BARCODE_SCANNER_UPC_E:
            UsageText := 'UPC-E';
          HID_USAGE_BARCODE_SCANNER_UPC_E1:
            UsageText := 'UPC-E1';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL:
            UsageText := 'Periodical';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_AUTODISCRIMINATE_2:
            UsageText := 'Periodical Auto-Discriminate +2';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_ONLY_DECODE_WITH_2:
            UsageText := 'Periodical Only Decode with +2';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_IGNORE_2:
            UsageText := 'Periodical Ignore +2';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_AUTODISCRIMINATE_5:
            UsageText := 'Periodical Auto-Discriminate +5';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_ONLY_DECODE_WITH_5:
            UsageText := 'Periodical Only Decode with +5';
          HID_USAGE_BARCODE_SCANNER_PERIODICAL_IGNORE_5:
            UsageText := 'Periodical Ignore +5';
          HID_USAGE_BARCODE_SCANNER_CHECK:
            UsageText := 'Check';
          HID_USAGE_BARCODE_SCANNER_CHECK_DISABLE_PRICE:
            UsageText := 'Check Disable Price';
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_4_DIGIT_PRICE:
            UsageText := 'Check Enable 4 Digit Price';
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_5_DIGIT_PRICE:
            UsageText := 'Check Enable 5 Digit Price';
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_EUROPEAN_4_DIGIT_PRICE:
            UsageText := 'Check Enable European 4 Digit Price';
          HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_EUROPEAN_5_DIGIT_PRICE:
            UsageText := 'Check Enable European 5 Digit Price';
          HID_USAGE_BARCODE_SCANNER_EAN_TWO_LABEL:
            UsageText := 'EAN Two Label';
          HID_USAGE_BARCODE_SCANNER_EAN_THREE_LABEL:
            UsageText := 'EAN Three Label';
          HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_1:
            UsageText := 'EAN 8 Flag Digit 1';
          HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_2:
            UsageText := 'EAN 8 Flag Digit 2';
          HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_3:
            UsageText := 'EAN 8 Flag Digit 3';
          HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_1:
            UsageText := 'EAN 13 Flag Digit 1';
          HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_2:
            UsageText := 'EAN 13 Flag Digit 2';
          HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_3:
            UsageText := 'EAN 13 Flag Digit 3';
          HID_USAGE_BARCODE_SCANNER_ADD_EAN_2_3_LABEL_DEFINITION:
            UsageText := 'Add EAN 2/3 Label Definition';
          HID_USAGE_BARCODE_SCANNER_CLEAR_ALL_EAN_2_3_LABEL_DEFINITIONS:
            UsageText := 'Clear All EAN 2/3 Label Definitions';
          HID_USAGE_BARCODE_SCANNER_CODABAR:
            UsageText := 'Codabar';
          HID_USAGE_BARCODE_SCANNER_CODE_128:
            UsageText := 'Code 128';
          HID_USAGE_BARCODE_SCANNER_CODE_39:
            UsageText := 'Code 39';
          HID_USAGE_BARCODE_SCANNER_CODE_93:
            UsageText := 'Code 93';
          HID_USAGE_BARCODE_SCANNER_FULL_ASCII_CONVERSION:
            UsageText := 'Full ASCII Conversion';
          HID_USAGE_BARCODE_SCANNER_INTERLEAVED_2_OF_5:
            UsageText := 'Interleaved 2 of 5';
          HID_USAGE_BARCODE_SCANNER_ITALIAN_PHARMACY_CODE:
            UsageText := 'Italian Pharmacy Code';
          HID_USAGE_BARCODE_SCANNER_MSI_PLESSEY:
            UsageText := 'MSI/Plessey';
          HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5_IATA:
            UsageText := 'Standard 2 of 5 IATA';
          HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5:
            UsageText := 'Standard 2 of 5';
          HID_USAGE_BARCODE_SCANNER_TRANSMIT_START_STOP:
            UsageText := 'Transmit Start/Stop';
          HID_USAGE_BARCODE_SCANNER_TRI_OPTIC:
            UsageText := 'Tri-Optic';
          HID_USAGE_BARCODE_SCANNER_UCC_EAN_128:
            UsageText := 'UCC/EAN-128';
          HID_USAGE_BARCODE_SCANNER_CHECK_DIGIT:
            UsageText := 'Check Digit';
          HID_USAGE_BARCODE_SCANNER_CD_DISABLE:
            UsageText := 'Check Digit Disable';
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_INTERLEAVED_2_OF_5_OPCC:
            UsageText := 'Check Digit Enable Interleaved 2 of 5 OPCC';
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_INTERLEAVED_2_OF_5_USS:
            UsageText := 'Check Digit Enable Interleaved 2 of 5 USS';
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_STANDARD_2_OF_5_OPCC:
            UsageText := 'Check Digit Enable Standard 2 of 5 OPCC';
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_STANDARD_2_OF_5_USS:
            UsageText := 'Check Digit Enable Standard 2 of 5 USS';
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_ONE_MSI_PLESSEY:
            UsageText := 'Check Digit Enable One MSI/Plessey';
          HID_USAGE_BARCODE_SCANNER_CD_ENABLE_TWO_MSI_PLESSEY:
            UsageText := 'Check Digit Enable Two MSI/Plessey';
          HID_USAGE_BARCODE_SCANNER_CD_CODABAR_ENABLE:
            UsageText := 'Check Digit Codabar Enable';
          HID_USAGE_BARCODE_SCANNER_CD_CODE_39_ENABLE:
            UsageText := 'Check Digit Code 39 Enable';
          HID_USAGE_BARCODE_SCANNER_TRANSMIT_CHECK_DIGIT:
            UsageText := 'Transmit Check Digit';
          HID_USAGE_BARCODE_SCANNER_DISABLE_CHECK_DIGIT_TRANSMIT:
            UsageText := 'Disable Check Digit Transmit';
          HID_USAGE_BARCODE_SCANNER_ENABLE_CHECK_DIGIT_TRANSMIT:
            UsageText := 'Enable Check Digit Transmit';
          HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_1:
            UsageText := 'Symbology Identifier 1';
          HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_2:
            UsageText := 'Symbology Identifier 2';
          HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_3:
            UsageText := 'Symbology Identifier 3';
          HID_USAGE_BARCODE_SCANNER_DECODED_DATA:
            UsageText := 'Decode Data';
          HID_USAGE_BARCODE_SCANNER_DECODED_DATA_CONTINUED:
            UsageText := 'Decoded Data Continued';
          HID_USAGE_BARCODE_SCANNER_BAR_SPACE_DATA:
            UsageText := 'Bar Space Data';
          HID_USAGE_BARCODE_SCANNER_SCANNER_DATA_ACCURACY:
            UsageText := 'Scanner Data Accuracy';
          HID_USAGE_BARCODE_SCANNER_RAW_DATA_POLARITY:
            UsageText := 'Raw Data Polarity';
          HID_USAGE_BARCODE_SCANNER_POLARITY_INVERTED_BAR_CODE:
            UsageText := 'Polarity Inverted Bar Code';
          HID_USAGE_BARCODE_SCANNER_POLARITY_NORMAL_BAR_CODE:
            UsageText := 'Polarity Normal Bar Code';
          HID_USAGE_BARCODE_SCANNER_MINIMUM_LENGTH_TO_DECODE:
            UsageText := 'Minimum Length to Decode';
          HID_USAGE_BARCODE_SCANNER_MAXIMUM_LENGTH_TO_DECODE:
            UsageText := 'Maximum Length to Decode';
          HID_USAGE_BARCODE_SCANNER_FIRST_DISCRETE_LENGTH_TO_DECODE:
            UsageText := 'First Discrete Length to Decode';
          HID_USAGE_BARCODE_SCANNER_SECOND_DISCRETE_LENGTH_TO_DECODE:
            UsageText := 'Second Discrete Length to Decode';
          HID_USAGE_BARCODE_SCANNER_DATA_LENGTH_METHOD:
            UsageText := 'Data Length Method';
          HID_USAGE_BARCODE_SCANNER_DLM_READ_ANY:
            UsageText := 'Data Length Method Read Any';
          HID_USAGE_BARCODE_SCANNER_DLM_CHECK_IN_RANGE:
            UsageText := 'Data Length Method Check in Range';
          HID_USAGE_BARCODE_SCANNER_DLM_CHECK_FOR_DISCRETE:
            UsageText := 'Data Length Method Check for Discrete';
          HID_USAGE_BARCODE_SCANNER_AZTEC_CODE:
            UsageText := 'Aztec Code';
          HID_USAGE_BARCODE_SCANNER_BC412:
            UsageText := 'BC412';
          HID_USAGE_BARCODE_SCANNER_CHANNEL_CODE:
            UsageText := 'Channel Code';
          HID_USAGE_BARCODE_SCANNER_CODE_16:
            UsageText := 'Code 16';
          HID_USAGE_BARCODE_SCANNER_CODE_32:
            UsageText := 'Code 32';
          HID_USAGE_BARCODE_SCANNER_CODE_49:
            UsageText := 'Code 49';
          HID_USAGE_BARCODE_SCANNER_CODE_ONE:
            UsageText := 'Code One';
          HID_USAGE_BARCODE_SCANNER_COLORCODE:
            UsageText := 'Colorcode';
          HID_USAGE_BARCODE_SCANNER_DATA_MATRIX:
            UsageText := 'Data Matrix';
          HID_USAGE_BARCODE_SCANNER_MAXICODE:
            UsageText := 'MaxiCode';
          HID_USAGE_BARCODE_SCANNER_MICROPDF:
            UsageText := 'MicroPDF';
          HID_USAGE_BARCODE_SCANNER_PDF_417:
            UsageText := 'PDF-417';
          HID_USAGE_BARCODE_SCANNER_POSICODE:
            UsageText := 'PosiCode';
          HID_USAGE_BARCODE_SCANNER_QR_CODE:
            UsageText := 'QR Code';
          HID_USAGE_BARCODE_SCANNER_SUPERCODE:
            UsageText := 'SuperCode';
          HID_USAGE_BARCODE_SCANNER_ULTRACODE:
            UsageText := 'UltraCode';
          HID_USAGE_BARCODE_SCANNER_USD_5:
            UsageText := 'USD-5 (Slug Code)';
          HID_USAGE_BARCODE_SCANNER_VERICODE:
            UsageText := 'VeriCode';
        end;
      end;
    HID_USAGE_PAGE_WEIGHING_DEVICE:
      begin
        UsagePageText := 'Weighing Device';
        case Usage of
          HID_USAGE_SCALE_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_SCALE_WEIGHING_DEVICE:
            UsageText := 'Weighing Device';
          HID_USAGE_SCALE_SCALE_DEVICE_CLASS:
            UsageText := 'Scale Device Class';
          HID_USAGE_SCALE_SCALE_CLASS_I_METRIC_CLASS:
            UsageText := 'Scale Class I Metric Class';
          HID_USAGE_SCALE_SCALE_CLASS_I_METRIC:
            UsageText := 'Scale Class I Metric';
          HID_USAGE_SCALE_SCALE_CLASS_II_METRIC:
            UsageText := 'Scale Class II Metric';
          HID_USAGE_SCALE_SCALE_CLASS_III_METRIC:
            UsageText := 'Scale Class III Metric';
          HID_USAGE_SCALE_SCALE_CLASS_IIIL_METRIC:
            UsageText := 'Scale Class IIIL Metric';
          HID_USAGE_SCALE_SCALE_CLASS_IV_METRIC:
            UsageText := 'Scale Class IV Metric';
          HID_USAGE_SCALE_SCALE_CLASS_III_ENGLISH:
            UsageText := 'Scale Class III English';
          HID_USAGE_SCALE_SCALE_CLASS_IIIL_ENGLISH:
            UsageText := 'Scale Class IIIL English';
          HID_USAGE_SCALE_SCALE_CLASS_IV_ENGLISH:
            UsageText := 'Scale Class IV English';
          HID_USAGE_SCALE_SCALE_CLASS_GENERIC:
            UsageText := 'Scale Class Generic';
          HID_USAGE_SCALE_SCALE_ATTRIBUTE_REPORT:
            UsageText := 'Scale Attribute Report';
          HID_USAGE_SCALE_SCALE_CONTROL_REPORT:
            UsageText := 'Scale Control Report';
          HID_USAGE_SCALE_SCALE_DATA_REPORT:
            UsageText := 'Scale Data Report';
          HID_USAGE_SCALE_SCALE_STATUS_REPORT:
            UsageText := 'Scale Status Report';
          HID_USAGE_SCALE_SCALE_WEIGHT_LIMIT_REPORT:
            UsageText := 'Scale Weight Limit Report';
          HID_USAGE_SCALE_SCALE_STATISTICS_REPORT:
            UsageText := 'Scale Statistics Report';
          HID_USAGE_SCALE_DATA_WEIGHT:
            UsageText := 'Data Weight';
          HID_USAGE_SCALE_DATA_SCALING:
            UsageText := 'Data Scaling';
          HID_USAGE_SCALE_WEIGHT_UNIT_CLASS:
            UsageText := 'Weight Unit Class';
          HID_USAGE_SCALE_WEIGHT_UNIT_MILLIGRAM:
            UsageText := 'Weight Unit Milligram';
          HID_USAGE_SCALE_WEIGHT_UNIT_GRAM:
            UsageText := 'Weight Unit Gram';
          HID_USAGE_SCALE_WEIGHT_UNIT_KILOGRAM:
            UsageText := 'Weight Unit Kilogram';
          HID_USAGE_SCALE_WEIGHT_UNIT_CARATS:
            UsageText := 'Weight Unit Carats';
          HID_USAGE_SCALE_WEIGHT_UNIT_TAELS:
            UsageText := 'Weight Unit Taels';
          HID_USAGE_SCALE_WEIGHT_UNIT_GRAINS:
            UsageText := 'Weight Unit Grains';
          HID_USAGE_SCALE_WEIGHT_UNIT_PENNYWEIGHTS:
            UsageText := 'Weight Unit Pennyweights';
          HID_USAGE_SCALE_WEIGHT_UNIT_METRIC_TON:
            UsageText := 'Weight Unit Metric Ton';
          HID_USAGE_SCALE_WEIGHT_UNIT_AVOIR_TON:
            UsageText := 'Weight Unit Avoir Ton';
          HID_USAGE_SCALE_WEIGHT_UNIT_TROY_OUNCE:
            UsageText := 'Weight Unit Troy Ounce';
          HID_USAGE_SCALE_WEIGHT_UNIT_OUNCE:
            UsageText := 'Weight Unit Ounce';
          HID_USAGE_SCALE_WEIGHT_UNIT_POUND:
            UsageText := 'Weight Unit Pound';
          HID_USAGE_SCALE_CALIBRATION_COUNT:
            UsageText := 'Calibration Count';
          HID_USAGE_SCALE_RE_ZERO_COUNT:
            UsageText := 'Re-Zero Count';
          HID_USAGE_SCALE_SCALE_STATUS_CLASS:
            UsageText := 'Scale Status Class';
          HID_USAGE_SCALE_SCS_FAULT:
            UsageText := 'Scale Status Fault';
          HID_USAGE_SCALE_SCS_STABLE_AT_CENTER_OF_ZERO:
            UsageText := 'Scale Status Stable at Center of Zero';
          HID_USAGE_SCALE_SCS_IN_MOTION:
            UsageText := 'Scale Status In Motion';
          HID_USAGE_SCALE_SCS_WEIGHT_STABLE:
            UsageText := 'Scale Status Weight Stable';
          HID_USAGE_SCALE_SCS_UNDER_ZERO:
            UsageText := 'Scale Status Under Zero';
          HID_USAGE_SCALE_SCS_OVER_WEIGHT_LIMIT:
            UsageText := 'Scale Status Over Weight Limit';
          HID_USAGE_SCALE_SCS_REQUIRES_CALIBRATION:
            UsageText := 'Scale Status Requires Calibration';
          HID_USAGE_SCALE_SCS_REQUIRES_REZEROING:
            UsageText := 'Scale Status Requires Rezeroing';
          HID_USAGE_SCALE_ZERO_SCALE:
            UsageText := 'Scale Status Zero Scale';
          HID_USAGE_SCALE_ENFORCED_ZERO_RETURN:
            UsageText := 'Scale Status Enforced Zero Return';
        end;
      end;
    HID_USAGE_PAGE_MAGNETIC_STRIPE_READER:
      begin
        UsagePageText := 'Magnetic Stripe Reader';
        case Usage of
          HID_USAGE_MSR_UNDEFINED:
            UsageText := 'Undefined';
          HID_USAGE_MSR_MSR_DEVICE_READ_ONLY:
            UsageText := 'MSR Device Read Only';
          HID_USAGE_MSR_TRACK_1_LENGTH:
            UsageText := 'Track 1 Length';
          HID_USAGE_MSR_TRACK_2_LENGTH:
            UsageText := 'Track 2 Length';
          HID_USAGE_MSR_TRACK_3_LENGTH:
            UsageText := 'Track 3 Length';
          HID_USAGE_MSR_TRACK_JIS_LENGTH:
            UsageText := 'Track JIS Length';
          HID_USAGE_MSR_TRACK_DATA:
            UsageText := 'Track Data';
          HID_USAGE_MSR_TRACK_1_DATA:
            UsageText := 'Track 1 Data';
          HID_USAGE_MSR_TRACK_2_DATA:
            UsageText := 'Track 2 Data';
          HID_USAGE_MSR_TRACK_3_DATA:
            UsageText := 'Track 3 Data';
          HID_USAGE_MSR_TRACK_JIS_DATA:
            UsageText := 'Track JIS Data';
        end;
      end;
  end;

  if UsagePageText = '' then
    UsagePageText := Format('%x', [UsagePage]);
  if UsageText = '' then
    UsageText := Format('%x', [Usage]);
end;

end.
