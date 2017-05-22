{******************************************************************}
{                                                                  }
{       Borland Delphi Runtime Library                             }
{       Public Definitions of HID USAGES                           }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (c) 1996, 1997 Microsoft Corporation                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: hidusage.h, released March 1999.           }
{ The original Pascal code is: HidUsage.pas, released 31 Jan 2000. }
{ The initial developer of the Pascal code is Robert Marquardt     }
{ (robert_marquardt att gmx dott de)                               }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (c) 1999, 2000 Robert Marquardt.                       }
{                                                                  }
{ Contributor(s): Marcel van Brakel (brakelm att chello dott nl)   }
{                 Francois KREBS (fkrebs att free dott fr)         }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit HidUsage;

interface

{$WEAKPACKAGEUNIT}

uses Windows;

const
  //
  // Usage Pages
  //
  HID_USAGE_PAGE_UNDEFINED                 = $00;
  HID_USAGE_PAGE_GENERIC                   = $01;
  HID_USAGE_PAGE_SIMULATION                = $02;
  HID_USAGE_PAGE_VR                        = $03;
  HID_USAGE_PAGE_SPORT                     = $04;
  HID_USAGE_PAGE_GAME                      = $05;
  HID_USAGE_PAGE_GENERIC_GAME_CONTROLS     = $06;
  HID_USAGE_PAGE_KEYBOARD                  = $07;
  HID_USAGE_PAGE_LED                       = $08;
  HID_USAGE_PAGE_BUTTON                    = $09;
  HID_USAGE_PAGE_ORDINAL                   = $0A;
  HID_USAGE_PAGE_TELEPHONY                 = $0B;
  HID_USAGE_PAGE_CONSUMER                  = $0C;
  HID_USAGE_PAGE_DIGITIZER                 = $0D;
  HID_USAGE_PAGE_PHYSICAL_INPUT_DEVICE     = $0F;
  HID_USAGE_PAGE_UNICODE                   = $10;
  HID_USAGE_PAGE_ALPHANUMERIC              = $14;

  HID_USAGE_PAGE_MEDICAL_INSTRUMENT        = $40;

  HID_USAGE_PAGE_USB_MONITOR               = $80;
  HID_USAGE_PAGE_MONITOR_ENUMERATED_VALUES = $81;
  HID_USAGE_PAGE_VESA_VIRTUAL_CONTROLS     = $82;
  HID_USAGE_PAGE_RESERVED                  = $83;
  HID_USAGE_PAGE_POWER_DEVICE              = $84;
  HID_USAGE_PAGE_BATTERY_SYSTEM            = $85;
  HID_USAGE_PAGE_BARCODE_SCANNER           = $8C;
  HID_USAGE_PAGE_WEIGHING_DEVICE           = $8D;
  HID_USAGE_PAGE_MAGNETIC_STRIPE_READER    = $8E;

  //
  // Usages from Generic Desktop Page (0x01)
  //
  HID_USAGE_UNDEFINED          = $00;
  HID_USAGE_GENERIC_POINTER    = $01;
  HID_USAGE_GENERIC_MOUSE      = $02;
  HID_USAGE_GENERIC_RESERVED1  = $03;
  HID_USAGE_GENERIC_JOYSTICK   = $04;
  HID_USAGE_GENERIC_GAMEPAD    = $05;
  HID_USAGE_GENERIC_KEYBOARD   = $06;
  HID_USAGE_GENERIC_KEYPAD     = $07;
  HID_USAGE_GENERIC_MULTIAXIS  = $08;

  HID_USAGE_GENERIC_X                   = $30;
  HID_USAGE_GENERIC_Y                   = $31;
  HID_USAGE_GENERIC_Z                   = $32;
  HID_USAGE_GENERIC_RX                  = $33;
  HID_USAGE_GENERIC_RY                  = $34;
  HID_USAGE_GENERIC_RZ                  = $35;
  HID_USAGE_GENERIC_SLIDER              = $36;
  HID_USAGE_GENERIC_DIAL                = $37;
  HID_USAGE_GENERIC_WHEEL               = $38;
  HID_USAGE_GENERIC_HATSWITCH           = $39;
  HID_USAGE_GENERIC_COUNTED_BUFFER      = $3A;
  HID_USAGE_GENERIC_BYTE_COUNT          = $3B;
  HID_USAGE_GENERIC_MOTION_WAKEUP       = $3C;
  HID_USAGE_GENERIC_START               = $3D;
  HID_USAGE_GENERIC_SELECT              = $3E;
  HID_USAGE_GENERIC_RESERVED2           = $3F;
  HID_USAGE_GENERIC_VX                  = $40;
  HID_USAGE_GENERIC_VY                  = $41;
  HID_USAGE_GENERIC_VZ                  = $42;
  HID_USAGE_GENERIC_VBRX                = $43;
  HID_USAGE_GENERIC_VBRY                = $44;
  HID_USAGE_GENERIC_VBRZ                = $45;
  HID_USAGE_GENERIC_VNO                 = $46;
  HID_USAGE_FEATURE_NOTIFICATION        = $47;
  HID_USAGE_GENERIC_SYSTEM_CTL          = $80;
  HID_USAGE_GENERIC_SYSCTL_POWER        = $81;
  HID_USAGE_GENERIC_SYSCTL_SLEEP        = $82;
  HID_USAGE_GENERIC_SYSCTL_WAKE         = $83;
  HID_USAGE_GENERIC_SYSCTL_CONTEXT_MENU = $84;
  HID_USAGE_GENERIC_SYSCTL_MAIN_MENU    = $85;
  HID_USAGE_GENERIC_SYSCTL_APP_MENU     = $86;
  HID_USAGE_GENERIC_SYSCTL_HELP_MENU    = $87;
  HID_USAGE_GENERIC_SYSCTL_MENU_EXIT    = $88;
  HID_USAGE_GENERIC_SYSCTL_MENU_SELECT  = $89;
  HID_USAGE_GENERIC_SYSCTL_MENU_RIGHT   = $8A;
  HID_USAGE_GENERIC_SYSCTL_MENU_LEFT    = $8B;
  HID_USAGE_GENERIC_SYSCTL_MENU_UP      = $8C;
  HID_USAGE_GENERIC_SYSCTL_MENU_DOWN    = $8D;
  HID_USAGE_GENERIC_SYSCTL_COLD_RESTART = $8E;
  HID_USAGE_GENERIC_SYSCTL_WARM_RESTART = $8F;
  HID_USAGE_GENERIC_SYSCTL_DPAD_UP      = $90;
  HID_USAGE_GENERIC_SYSCTL_DPAD_DOWN    = $91;
  HID_USAGE_GENERIC_SYSCTL_DPAD_RIGHT   = $92;
  HID_USAGE_GENERIC_SYSCTL_DPAD_LEFT    = $93;

  HID_USAGE_GENERIC_SYSCTL_DOCK                   = $A0;
  HID_USAGE_GENERIC_SYSCTL_UNDOCK                 = $A1;
  HID_USAGE_GENERIC_SYSCTL_SETUP                  = $A2;
  HID_USAGE_GENERIC_SYSCTL_BREAK                  = $A3;
  HID_USAGE_GENERIC_SYSCTL_DEBUGGER_BREAK         = $A4;
  HID_USAGE_GENERIC_SYSCTL_APP_BREAK              = $A5;
  HID_USAGE_GENERIC_SYSCTL_APP_DEBUGGER_BREAK     = $A6;
  HID_USAGE_GENERIC_SYSCTL_SYSTEM_SPEAKER_MUTE    = $A7;
  HID_USAGE_GENERIC_SYSCTL_SYSTEM_HIBERNATE       = $A8;

  HID_USAGE_GENERIC_SYSCTL_DISPLAY_INVERT         = $B0;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_INTERNAL       = $B1;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_EXTERNAL       = $B2;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_BOTH           = $B3;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_DUAL           = $B4;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_TOGGLE_INT_EXT = $B5;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_SWAP           = $B6;
  HID_USAGE_GENERIC_SYSCTL_DISPLAY_LCD_AUTOSCALE  = $B7;

  //
  // Usages from Simulation Controls Page (0x02)
  //
  HID_USAGE_SIMULATION_UNDEFINED                  = $00;
  HID_USAGE_SIMULATION_FLIGHT                     = $01;
  HID_USAGE_SIMULATION_AUTOMOBILE                 = $02;
  HID_USAGE_SIMULATION_TANK                       = $03;
  HID_USAGE_SIMULATION_SPACESHIP                  = $04;
  HID_USAGE_SIMULATION_SUBMARINE                  = $05;
  HID_USAGE_SIMULATION_SAILING                    = $06;
  HID_USAGE_SIMULATION_MOTORCYCLE                 = $07;
  HID_USAGE_SIMULATION_SPORTS                     = $08;
  HID_USAGE_SIMULATION_AIRPLANE                   = $09;
  HID_USAGE_SIMULATION_HELICOPTER                 = $0A;
  HID_USAGE_SIMULATION_MAGIC_CARPET               = $0B;
  HID_USAGE_SIMULATION_BICYCLE                    = $0C;
  HID_USAGE_SIMULATION_FLIGHT_CONTROL_STICK       = $20;
  HID_USAGE_SIMULATION_FLIGHT_STICK               = $21;
  HID_USAGE_SIMULATION_CYCLIC_CONTROL             = $22;
  HID_USAGE_SIMULATION_CYCLIC_TRIM                = $23;
  HID_USAGE_SIMULATION_FLIGHT_YOKE                = $24;
  HID_USAGE_SIMULATION_TRACK_CONTROL              = $25;
  HID_USAGE_SIMULATION_AILERON                    = $B0;
  HID_USAGE_SIMULATION_AILERON_TRIM               = $B1;
  HID_USAGE_SIMULATION_ANTITORQUE_CONTROL         = $B2;
  HID_USAGE_SIMULATION_AUTOPILOT_ENABLE           = $B3;
  HID_USAGE_SIMULATION_CHAFF_RELEASE              = $B4;
  HID_USAGE_SIMULATION_COLLECTIVE_CONTROL         = $B5;
  HID_USAGE_SIMULATION_DIVE_BREAK                 = $B6;
  HID_USAGE_SIMULATION_ELECTRONIC_COUNTERMEASURES = $B7;
  HID_USAGE_SIMULATION_ELEVATOR                   = $B8;
  HID_USAGE_SIMULATION_ELEVATOR_TRIM              = $B9;
  HID_USAGE_SIMULATION_RUDDER                     = $BA;
  HID_USAGE_SIMULATION_THROTTLE                   = $BB;
  HID_USAGE_SIMULATION_FLIGHT_COMMUNICATIONS      = $BC;
  HID_USAGE_SIMULATION_FLARE_RELEASE              = $BD;
  HID_USAGE_SIMULATION_LANDING_GEAR               = $BE;
  HID_USAGE_SIMULATION_TOE_BRAKE                  = $BF;
  HID_USAGE_SIMULATION_TRIGGER                    = $C0;
  HID_USAGE_SIMULATION_WEAPONS_ARM                = $C1;
  HID_USAGE_SIMULATION_WEAPONS_SELECT             = $C2;
  HID_USAGE_SIMULATION_WING_FLAPS                 = $C3;
  HID_USAGE_SIMULATION_ACCELERATOR                = $C4;
  HID_USAGE_SIMULATION_BRAKE                      = $C5;
  HID_USAGE_SIMULATION_CLUTCH                     = $C6;
  HID_USAGE_SIMULATION_SHIFTER                    = $C7;
  HID_USAGE_SIMULATION_STEERING                   = $C8;
  HID_USAGE_SIMULATION_TURRET_DIRECTION           = $C9;
  HID_USAGE_SIMULATION_BARREL_ELEVATION           = $CA;
  HID_USAGE_SIMULATION_DIVE_PLANE                 = $CB;
  HID_USAGE_SIMULATION_BALLAST                    = $CC;
  HID_USAGE_SIMULATION_BICYCLE_CRANK              = $CD;
  HID_USAGE_SIMULATION_HANDLE_BARS                = $CE;
  HID_USAGE_SIMULATION_FRONT_BRAKE                = $CF;
  HID_USAGE_SIMULATION_REAR_BRAKE                 = $D0;

  //
  // Virtual Reality Controls Page (0x03)
  //
  HID_USAGE_VR_UNDEFINED             = $00;
  HID_USAGE_VR_BELT                  = $01;
  HID_USAGE_VR_BODY_SUIT             = $02;
  HID_USAGE_VR_FLEXOR                = $03;
  HID_USAGE_VR_GLOVE                 = $04;
  HID_USAGE_VR_HEAD_TRACKER          = $05;
  HID_USAGE_VR_HEAD_MOUNTED_DISPLAY  = $06;
  HID_USAGE_VR_HAND_TRACKER          = $07;
  HID_USAGE_VR_OCULOMETER            = $08;
  HID_USAGE_VR_VEST                  = $09;
  HID_USAGE_VR_ANIMATRONIC_DEVICE    = $0A;

  HID_USAGE_VR_STEREO_ENABLE         = $20;
  HID_USAGE_VR_DISPLAY_ENABLE        = $21;

  //
  // Sport Controls Page (0x04)
  //
  HID_USAGE_SPORT_UNDEFINED            = $00;
  HID_USAGE_SPORT_BASEBALL_BAT         = $01;
  HID_USAGE_SPORT_GOLF_CLUB            = $02;
  HID_USAGE_SPORT_ROWING_MACHINE       = $03;
  HID_USAGE_SPORT_TREADMILL            = $04;

  HID_USAGE_SPORT_OAR                  = $30;
  HID_USAGE_SPORT_SLOPE                = $31;
  HID_USAGE_SPORT_RATE                 = $32;
  HID_USAGE_SPORT_STICK_SPEED          = $33;
  HID_USAGE_SPORT_STICK_FACE_ANGLE     = $34;
  HID_USAGE_SPORT_STICK_HEEL_TOE       = $35;
  HID_USAGE_SPORT_STICK_FOLLOW_THROUGH = $36;
  HID_USAGE_SPORT_STICK_TEMPO          = $37;
  HID_USAGE_SPORT_STICK_TYPE           = $38;
  HID_USAGE_SPORT_STICK_HEIGHT         = $39;

  HID_USAGE_SPORT_PUTTER               = $50;
  HID_USAGE_SPORT_IRON_1               = $51;
  HID_USAGE_SPORT_IRON_2               = $52;
  HID_USAGE_SPORT_IRON_3               = $53;
  HID_USAGE_SPORT_IRON_4               = $54;
  HID_USAGE_SPORT_IRON_5               = $55;
  HID_USAGE_SPORT_IRON_6               = $56;
  HID_USAGE_SPORT_IRON_7               = $57;
  HID_USAGE_SPORT_IRON_8               = $58;
  HID_USAGE_SPORT_IRON_9               = $59;
  HID_USAGE_SPORT_IRON_10              = $5A;
  HID_USAGE_SPORT_IRON_11              = $5B;
  HID_USAGE_SPORT_SAND_WEDGE           = $5C;
  HID_USAGE_SPORT_LOFT_WEDGE           = $5D;
  HID_USAGE_SPORT_POWER_WEDGE          = $5E;
  HID_USAGE_SPORT_WOOD_1               = $5F;
  HID_USAGE_SPORT_WOOD_3               = $60;
  HID_USAGE_SPORT_WOOD_5               = $61;
  HID_USAGE_SPORT_WOOD_7               = $62;
  HID_USAGE_SPORT_WOOD_9               = $63;

  //
  // Game Controls Page (0x05)
  //
  HID_USAGE_GAME_UNDEFINED              = $00;
  HID_USAGE_GAME_3D_GAME_CONTROLLER     = $01;
  HID_USAGE_GAME_PINBALL_DEVICE         = $02;
  HID_USAGE_GAME_GUN_DEVICE             = $03;

  HID_USAGE_GAME_POINT_OF_VIEW          = $20;
  HID_USAGE_GAME_TURN_RIGHT_LEFT        = $21;
  HID_USAGE_GAME_PITCH_FORWARD_BACKWARD = $22;
  HID_USAGE_GAME_ROLL_RIGHT_LEFT        = $23;
  HID_USAGE_GAME_MOVE_RIGHT_LEFT        = $24;
  HID_USAGE_GAME_MOVE_FORWARD_BACKWARD  = $25;
  HID_USAGE_GAME_MOVE_UP_DOWN           = $26;
  HID_USAGE_GAME_LEAN_RIGHT_LEFT        = $27;
  HID_USAGE_GAME_LEAN_FORWARD_BACKWARD  = $28;
  HID_USAGE_GAME_HEIGHT_OF_POV          = $29;
  HID_USAGE_GAME_FLIPPER                = $2A;
  HID_USAGE_GAME_SECONDARY_FLIPPER      = $2B;
  HID_USAGE_GAME_BUMP                   = $2C;
  HID_USAGE_GAME_NEW_GAME               = $2D;
  HID_USAGE_GAME_SHOOT_BALL             = $2E;
  HID_USAGE_GAME_PLAYER                 = $2F;
  HID_USAGE_GAME_GUN_BOLT               = $30;
  HID_USAGE_GAME_GUN_CLIP               = $31;
  HID_USAGE_GAME_GUN_SELECTOR           = $32;
  HID_USAGE_GAME_GUN_SINGLE_SHOT        = $33;
  HID_USAGE_GAME_GUN_BURST              = $34;
  HID_USAGE_GAME_GUN_AUTOMATIC          = $35;
  HID_USAGE_GAME_GUN_SAFETY             = $36;
  HID_USAGE_GAME_GAMEPAD_FIRE_JUMP      = $37;

  HID_USAGE_GAME_GAMEPAD_TRIGGER        = $39;

  //
  // Generic Device Controls Page (0x06)
  //
  HID_USAGE_GENERIC_GAME_UNDEFINED        = $00;
  HID_USAGE_GENERIC_GAME_BATTERY_STRENGTH = $20;
  HID_USAGE_GENERIC_GAME_WIRELESS_CHANNEL = $21;
  HID_USAGE_GENERIC_GAME_WIRELESS_ID      = $22;

  //
  // Keyboard/Keypad Page (0x07)
  //

  // Error "keys"
  HID_USAGE_KEYBOARD_NOEVENT      = $00;
  HID_USAGE_KEYBOARD_ROLLOVER     = $01;
  HID_USAGE_KEYBOARD_POSTFAIL     = $02;
  HID_USAGE_KEYBOARD_UNDEFINED    = $03;

  // Letters
  HID_USAGE_KEYBOARD_aA           = $04;
  HID_USAGE_KEYBOARD_bB           = $05;
  HID_USAGE_KEYBOARD_cC           = $06;
  HID_USAGE_KEYBOARD_dD           = $07;
  HID_USAGE_KEYBOARD_eE           = $08;
  HID_USAGE_KEYBOARD_fF           = $09;
  HID_USAGE_KEYBOARD_gG           = $0A;
  HID_USAGE_KEYBOARD_hH           = $0B;
  HID_USAGE_KEYBOARD_iI           = $0C;
  HID_USAGE_KEYBOARD_jJ           = $0D;
  HID_USAGE_KEYBOARD_kK           = $0E;
  HID_USAGE_KEYBOARD_lL           = $0F;
  HID_USAGE_KEYBOARD_mM           = $10;
  HID_USAGE_KEYBOARD_nN           = $11;
  HID_USAGE_KEYBOARD_oO           = $12;
  HID_USAGE_KEYBOARD_pP           = $13;
  HID_USAGE_KEYBOARD_qQ           = $14;
  HID_USAGE_KEYBOARD_rR           = $15;
  HID_USAGE_KEYBOARD_sS           = $16;
  HID_USAGE_KEYBOARD_tT           = $17;
  HID_USAGE_KEYBOARD_uU           = $18;
  HID_USAGE_KEYBOARD_vV           = $19;
  HID_USAGE_KEYBOARD_wW           = $1A;
  HID_USAGE_KEYBOARD_xX           = $1B;
  HID_USAGE_KEYBOARD_yY           = $1C;
  HID_USAGE_KEYBOARD_zZ           = $1D;

  // Numbers
  HID_USAGE_KEYBOARD_ONE          = $1E; // or !
  HID_USAGE_KEYBOARD_TWO          = $1F; // or @
  HID_USAGE_KEYBOARD_THREE        = $20; // or #
  HID_USAGE_KEYBOARD_FOUR         = $21; // or $
  HID_USAGE_KEYBOARD_FIVE         = $22; // or %
  HID_USAGE_KEYBOARD_SIX          = $23; // or ^
  HID_USAGE_KEYBOARD_SEVEN        = $24; // or &
  HID_USAGE_KEYBOARD_EIGHT        = $25; // or *
  HID_USAGE_KEYBOARD_NINE         = $26; // or (
  HID_USAGE_KEYBOARD_ZERO         = $27; // or )

  HID_USAGE_KEYBOARD_ENTER        = $28; // RETURN is another key
  HID_USAGE_KEYBOARD_ESCAPE       = $29;
  HID_USAGE_KEYBOARD_BACKSPACE    = $2A; // Delete left char
  HID_USAGE_KEYBOARD_TAB          = $2B;
  HID_USAGE_KEYBOARD_SPACE        = $2C;
  HID_USAGE_KEYBOARD_MINUS        = $2D; // or _
  HID_USAGE_KEYBOARD_EQUAL        = $2E; // or +
  HID_USAGE_KEYBOARD_LSQBRACKET   = $2F; // or {
  HID_USAGE_KEYBOARD_RSQBRACKET   = $30; // or }
  HID_USAGE_KEYBOARD_BACKSLASH    = $31; // or |
  HID_USAGE_KEYBOARD_HASHMARK2    = $32; // or ~ Non US Key
  HID_USAGE_KEYBOARD_SEMICOLON    = $33;
  HID_USAGE_KEYBOARD_APOSTROPH    = $34; // or :
  HID_USAGE_KEYBOARD_GRAVEACCENT  = $35; // or Tilde
  HID_USAGE_KEYBOARD_COMMA        = $36; // or <
  HID_USAGE_KEYBOARD_DOT          = $37; // or >
  HID_USAGE_KEYBOARD_SLASH        = $38; // or ?

  HID_USAGE_KEYBOARD_CAPS_LOCK    = $39;

  // Function keys
  HID_USAGE_KEYBOARD_F1           = $3A;
  HID_USAGE_KEYBOARD_F2           = $3B;
  HID_USAGE_KEYBOARD_F3           = $3C;
  HID_USAGE_KEYBOARD_F4           = $3D;
  HID_USAGE_KEYBOARD_F5           = $3E;
  HID_USAGE_KEYBOARD_F6           = $3F;
  HID_USAGE_KEYBOARD_F7           = $40;
  HID_USAGE_KEYBOARD_F8           = $41;
  HID_USAGE_KEYBOARD_F9           = $42;
  HID_USAGE_KEYBOARD_F10          = $43;
  HID_USAGE_KEYBOARD_F11          = $44;
  HID_USAGE_KEYBOARD_F12          = $45;

  HID_USAGE_KEYBOARD_PRINT_SCREEN = $46;
  HID_USAGE_KEYBOARD_SCROLL_LOCK  = $47;

  HID_USAGE_KEYBOARD_PAUSE        = $48;
  HID_USAGE_KEYBOARD_INSERT       = $49;
  HID_USAGE_KEYBOARD_HOME         = $4A;
  HID_USAGE_KEYBOARD_PAGEUP       = $4B;
  HID_USAGE_KEYBOARD_DELETE       = $4C;
  HID_USAGE_KEYBOARD_END          = $4D;
  HID_USAGE_KEYBOARD_PAGEDOWN     = $4E;
  HID_USAGE_KEYBOARD_RIGHT        = $4F;
  HID_USAGE_KEYBOARD_LEFT         = $50;
  HID_USAGE_KEYBOARD_DOWN         = $51;
  HID_USAGE_KEYBOARD_UP           = $52;

  HID_USAGE_KEYPAD_NUM_LOCK       = $53;
  HID_USAGE_KEYPAD_SLASH          = $54;
  HID_USAGE_KEYPAD_STAR           = $55;
  HID_USAGE_KEYPAD_MINUS          = $56;
  HID_USAGE_KEYPAD_PLUS           = $57;
  HID_USAGE_KEYPAD_ENTER          = $58;
  HID_USAGE_KEYPAD_ONE            = $59;
  HID_USAGE_KEYPAD_TWO            = $5A;
  HID_USAGE_KEYPAD_THREE          = $5B;
  HID_USAGE_KEYPAD_FOUR           = $5C;
  HID_USAGE_KEYPAD_FIVE           = $5D;
  HID_USAGE_KEYPAD_SIX            = $5E;
  HID_USAGE_KEYPAD_SEVEN          = $5F;
  HID_USAGE_KEYPAD_EIGHT          = $60;
  HID_USAGE_KEYPAD_NINE           = $61;
  HID_USAGE_KEYPAD_ZERO           = $62;
  HID_USAGE_KEYPAD_DOT            = $63;

  HID_USAGE_KEYBOARD_BACKSLASH2   = $64; // or | Non US key

  HID_USAGE_KEYBOARD_APPLICATION  = $65;

  // Keys not for Windows
  HID_USAGE_KEYBOARD_POWER        = $66;
  HID_USAGE_KEYPAD_EQUAL2         = $67;

  // Keys not for Windows
  HID_USAGE_KEYBOARD_F13          = $68;
  HID_USAGE_KEYBOARD_F14          = $69;
  HID_USAGE_KEYBOARD_F15          = $6A;
  HID_USAGE_KEYBOARD_F16          = $6B;
  HID_USAGE_KEYBOARD_F17          = $6C;
  HID_USAGE_KEYBOARD_F18          = $6D;
  HID_USAGE_KEYBOARD_F19          = $6E;
  HID_USAGE_KEYBOARD_F20          = $6F;
  HID_USAGE_KEYBOARD_F21          = $70;
  HID_USAGE_KEYBOARD_F22          = $71;
  HID_USAGE_KEYBOARD_F23          = $72;
  HID_USAGE_KEYBOARD_F24          = $73;
  HID_USAGE_KEYBOARD_EXECUTE      = $74;
  HID_USAGE_KEYBOARD_HELP         = $75;
  HID_USAGE_KEYBOARD_MENU         = $76;
  HID_USAGE_KEYBOARD_SELECT       = $77;
  HID_USAGE_KEYBOARD_STOP         = $78;
  HID_USAGE_KEYBOARD_AGAIN        = $79;
  HID_USAGE_KEYBOARD_UNDO         = $7A;
  HID_USAGE_KEYBOARD_CUT          = $7B;
  HID_USAGE_KEYBOARD_COPY         = $7C;
  HID_USAGE_KEYBOARD_PASTE        = $7D;
  HID_USAGE_KEYBOARD_FIND         = $7E;
  HID_USAGE_KEYBOARD_MUTE         = $7F;
  HID_USAGE_KEYBOARD_VOLUME_UP    = $80;
  HID_USAGE_KEYBOARD_VOLUME_DOWN  = $81;

  HID_USAGE_KEYBOARD_LOCKCAPS     = $82;
  HID_USAGE_KEYBOARD_LOCKNUM      = $83;
  HID_USAGE_KEYBOARD_LOCKSCROLL   = $84;

  HID_USAGE_KEYPAD_COMMA          = $85;
  HID_USAGE_KEYPAD_EQUALSIGN      = $86;

  HID_USAGE_KEYBOARD_INATL1       = $87;
  HID_USAGE_KEYBOARD_INATL2       = $88;
  HID_USAGE_KEYBOARD_INATL3       = $89;
  HID_USAGE_KEYBOARD_INATL4       = $8A;
  HID_USAGE_KEYBOARD_INATL5       = $8B;
  HID_USAGE_KEYBOARD_INATL6       = $8C;
  HID_USAGE_KEYBOARD_INATL7       = $8D;
  HID_USAGE_KEYBOARD_INATL8       = $8E;
  HID_USAGE_KEYBOARD_INATL9       = $8F;
  HID_USAGE_KEYBOARD_LANG1        = $90;
  HID_USAGE_KEYBOARD_LANG2        = $91;
  HID_USAGE_KEYBOARD_LANG3        = $92;
  HID_USAGE_KEYBOARD_LANG4        = $93;
  HID_USAGE_KEYBOARD_LANG5        = $94;
  HID_USAGE_KEYBOARD_LANG6        = $95;
  HID_USAGE_KEYBOARD_LANG7        = $96;
  HID_USAGE_KEYBOARD_LANG8        = $97;
  HID_USAGE_KEYBOARD_LANG9        = $98;

  HID_USAGE_KEYBOARD_ALTERASE     = $99;
  HID_USAGE_KEYBOARD_SYSREQ       = $9A;
  HID_USAGE_KEYBOARD_CANCEL       = $9B;
  HID_USAGE_KEYBOARD_CLEAR        = $9C;
  HID_USAGE_KEYBOARD_PRIOR        = $9D;
  HID_USAGE_KEYBOARD_RETURN       = $9E;
  HID_USAGE_KEYBOARD_SEPARATOR    = $9F;
  HID_USAGE_KEYBOARD_OUT          = $A0;
  HID_USAGE_KEYBOARD_OPER         = $A1;
  HID_USAGE_KEYBOARD_CLEAR_AGAIN  = $A2;
  HID_USAGE_KEYBOARD_CRSEL        = $A3;
  HID_USAGE_KEYBOARD_EXSEL        = $A4;

  HID_USAGE_KEYPAD_HUNDREDS       = $B0;
  HID_USAGE_KEYPAD_THOUSANDS      = $B1;
  HID_USAGE_KEYPAD_THOUSANDS_SEP  = $B2;
  HID_USAGE_KEYPAD_DECIMAL_SEP    = $B3;
  HID_USAGE_KEYPAD_CURR_UNIT      = $B4;
  HID_USAGE_KEYPAD_CURR_SUBUNIT   = $B5;
  HID_USAGE_KEYPAD_LROUNDBRACKET  = $B6;
  HID_USAGE_KEYPAD_RROUNDBRACKET  = $B7;
  HID_USAGE_KEYPAD_LCURLYBRACKET  = $B8;
  HID_USAGE_KEYPAD_RCURLYBRACKET  = $B9;
  HID_USAGE_KEYPAD_TABULATOR      = $BA;
  HID_USAGE_KEYPAD_BACKSPACE      = $BB;
  HID_USAGE_KEYPAD_A              = $BC;
  HID_USAGE_KEYPAD_B              = $BD;
  HID_USAGE_KEYPAD_C              = $BE;
  HID_USAGE_KEYPAD_D              = $BF;
  HID_USAGE_KEYPAD_E              = $C0;
  HID_USAGE_KEYPAD_F              = $C1;
  HID_USAGE_KEYPAD_XOR            = $C2;
  HID_USAGE_KEYPAD_CIRCUMFLEX     = $C3;
  HID_USAGE_KEYPAD_PERCENT        = $C4;
  HID_USAGE_KEYPAD_BIGGER_THAN    = $C5;
  HID_USAGE_KEYPAD_LESS_THAN      = $C6;
  HID_USAGE_KEYPAD_BINARY_AND     = $C7;
  HID_USAGE_KEYPAD_LOGICAL_AND    = $C8;
  HID_USAGE_KEYPAD_BINARY_OR      = $C9;
  HID_USAGE_KEYPAD_LOGICAL_OR     = $CA;
  HID_USAGE_KEYPAD_COLON          = $CB;
  HID_USAGE_KEYPAD_HASHMARK       = $CC;
  HID_USAGE_KEYPAD_SPACE          = $CD;
  HID_USAGE_KEYPAD_AT             = $CE;
  HID_USAGE_KEYPAD_EXCLAMATION    = $CF;
  HID_USAGE_KEYPAD_MEM_STORE      = $D0;
  HID_USAGE_KEYPAD_MEM_RECALL     = $D1;
  HID_USAGE_KEYPAD_MEM_CLEAR      = $D2;
  HID_USAGE_KEYPAD_MEM_ADD        = $D3;
  HID_USAGE_KEYPAD_MEM_SUBTRACT   = $D4;
  HID_USAGE_KEYPAD_MEM_MULTIPLY   = $D5;
  HID_USAGE_KEYPAD_MEM_DIVIDE     = $D6;
  HID_USAGE_KEYPAD_PLUS_MINUS     = $D7;
  HID_USAGE_KEYPAD_CLEAR          = $D8;
  HID_USAGE_KEYPAD_CLEAR_ENTRY    = $D9;
  HID_USAGE_KEYPAD_BINARY         = $DA;
  HID_USAGE_KEYPAD_OCTAL          = $DB;
  HID_USAGE_KEYPAD_DECIMAL        = $DC;
  HID_USAGE_KEYPAD_HEXADECIMAL    = $DD;
  HID_USAGE_KEYPAD_RESERVED1      = $DE;
  HID_USAGE_KEYPAD_RESERVED2      = $DF;

  HID_USAGE_KEYBOARD_LCTRL        = $E0;
  HID_USAGE_KEYBOARD_LSHFT        = $E1;
  HID_USAGE_KEYBOARD_LALT         = $E2;
  HID_USAGE_KEYBOARD_LGUI         = $E3;
  HID_USAGE_KEYBOARD_RCTRL        = $E4;
  HID_USAGE_KEYBOARD_RSHFT        = $E5;
  HID_USAGE_KEYBOARD_RALT         = $E6;
  HID_USAGE_KEYBOARD_RGUI         = $E7;

  // and hundreds more...
  // (rom) $E8 to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  // LED Page (0x08)
  //
  HID_USAGE_LED_UNDEFINED            = $00;
  HID_USAGE_LED_NUM_LOCK             = $01;
  HID_USAGE_LED_CAPS_LOCK            = $02;
  HID_USAGE_LED_SCROLL_LOCK          = $03;
  HID_USAGE_LED_COMPOSE              = $04;
  HID_USAGE_LED_KANA                 = $05;
  HID_USAGE_LED_POWER                = $06;
  HID_USAGE_LED_SHIFT                = $07;
  HID_USAGE_LED_DO_NOT_DISTURB       = $08;
  HID_USAGE_LED_MUTE                 = $09;
  HID_USAGE_LED_TONE_ENABLE          = $0A;
  HID_USAGE_LED_HIGH_CUT_FILTER      = $0B;
  HID_USAGE_LED_LOW_CUT_FILTER       = $0C;
  HID_USAGE_LED_EQUALIZER_ENABLE     = $0D;
  HID_USAGE_LED_SOUND_FIELD_ON       = $0E;
  HID_USAGE_LED_SURROUND_FIELD_ON    = $0F;
  HID_USAGE_LED_REPEAT               = $10;
  HID_USAGE_LED_STEREO               = $11;
  HID_USAGE_LED_SAMPLING_RATE_DETECT = $12;
  HID_USAGE_LED_SPINNING             = $13;
  HID_USAGE_LED_CAV                  = $14;
  HID_USAGE_LED_CLV                  = $15;
  HID_USAGE_LED_RECORDING_FORMAT_DET = $16;
  HID_USAGE_LED_OFF_HOOK             = $17;
  HID_USAGE_LED_RING                 = $18;
  HID_USAGE_LED_MESSAGE_WAITING      = $19;
  HID_USAGE_LED_DATA_MODE            = $1A;
  HID_USAGE_LED_BATTERY_OPERATION    = $1B;
  HID_USAGE_LED_BATTERY_OK           = $1C;
  HID_USAGE_LED_BATTERY_LOW          = $1D;
  HID_USAGE_LED_SPEAKER              = $1E;
  HID_USAGE_LED_HEAD_SET             = $1F;
  HID_USAGE_LED_HOLD                 = $20;
  HID_USAGE_LED_MICROPHONE           = $21;
  HID_USAGE_LED_COVERAGE             = $22;
  HID_USAGE_LED_NIGHT_MODE           = $23;
  HID_USAGE_LED_SEND_CALLS           = $24;
  HID_USAGE_LED_CALL_PICKUP          = $25;
  HID_USAGE_LED_CONFERENCE           = $26;
  HID_USAGE_LED_STAND_BY             = $27;
  HID_USAGE_LED_CAMERA_ON            = $28;
  HID_USAGE_LED_CAMERA_OFF           = $29;
  HID_USAGE_LED_ON_LINE              = $2A;
  HID_USAGE_LED_OFF_LINE             = $2B;
  HID_USAGE_LED_BUSY                 = $2C;
  HID_USAGE_LED_READY                = $2D;
  HID_USAGE_LED_PAPER_OUT            = $2E;
  HID_USAGE_LED_PAPER_JAM            = $2F;
  HID_USAGE_LED_REMOTE               = $30;
  HID_USAGE_LED_FORWARD              = $31;
  HID_USAGE_LED_REVERSE              = $32;
  HID_USAGE_LED_STOP                 = $33;
  HID_USAGE_LED_REWIND               = $34;
  HID_USAGE_LED_FAST_FORWARD         = $35;
  HID_USAGE_LED_PLAY                 = $36;
  HID_USAGE_LED_PAUSE                = $37;
  HID_USAGE_LED_RECORD               = $38;
  HID_USAGE_LED_ERROR                = $39;
  HID_USAGE_LED_SELECTED_INDICATOR   = $3A;
  HID_USAGE_LED_IN_USE_INDICATOR     = $3B;
  HID_USAGE_LED_MULTI_MODE_INDICATOR = $3C;
  HID_USAGE_LED_INDICATOR_ON         = $3D;
  HID_USAGE_LED_INDICATOR_FLASH      = $3E;
  HID_USAGE_LED_INDICATOR_SLOW_BLINK = $3F;
  HID_USAGE_LED_INDICATOR_FAST_BLINK = $40;
  HID_USAGE_LED_INDICATOR_OFF        = $41;
  HID_USAGE_LED_FLASH_ON_TIME        = $42;
  HID_USAGE_LED_SLOW_BLINK_ON_TIME   = $43;
  HID_USAGE_LED_SLOW_BLINK_OFF_TIME  = $44;
  HID_USAGE_LED_FAST_BLINK_ON_TIME   = $45;
  HID_USAGE_LED_FAST_BLINK_OFF_TIME  = $46;
  HID_USAGE_LED_INDICATOR_COLOR      = $47;
  HID_USAGE_LED_RED                  = $48;
  HID_USAGE_LED_GREEN                = $49;
  HID_USAGE_LED_AMBER                = $4A;
  HID_USAGE_LED_GENERIC_INDICATOR    = $4B;
  HID_USAGE_LED_SYSTEM_SUSPEND       = $4C;
  HID_USAGE_LED_EXTERNAL_POWER       = $4D;

  // (rom) $4E to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  //  Button Page (0x09)
  //
  //  There is no need to label these usages.
  //
  HID_USAGE_BUTTON_NO_BUTTON = $00;  // (rom) Usage 1..65535 is the button number

  //
  //  Ordinal Page (0x0A)
  //
  //  There is no need to label these usages.
  //
  HID_USAGE_ORDINAL_RESERVED = $00;  // (rom) Usage 1..65535 is the ordinal number

  //
  //  Telephony Device Page (0x0B)
  //
  HID_USAGE_TELEPHONY_UNDEFINED           = $00;
  HID_USAGE_TELEPHONY_PHONE               = $01;
  HID_USAGE_TELEPHONY_ANSWERING_MACHINE   = $02;
  HID_USAGE_TELEPHONY_MESSAGE_CONTROLS    = $03;
  HID_USAGE_TELEPHONY_HANDSET             = $04;
  HID_USAGE_TELEPHONY_HEADSET             = $05;
  HID_USAGE_TELEPHONY_KEYPAD              = $06;
  HID_USAGE_TELEPHONY_PROGRAMMABLE_BUTTON = $07;

  HID_USAGE_TELEPHONY_HOOK_SWITCH         = $20;
  HID_USAGE_TELEPHONY_FLASH               = $21;
  HID_USAGE_TELEPHONY_FEATURE             = $22;
  HID_USAGE_TELEPHONY_HOLD                = $23;
  HID_USAGE_TELEPHONY_REDIAL              = $24;
  HID_USAGE_TELEPHONY_TRANSFER            = $25;
  HID_USAGE_TELEPHONY_DROP                = $26;
  HID_USAGE_TELEPHONY_PARK                = $27;
  HID_USAGE_TELEPHONY_FORWARD_CALLS       = $28;
  HID_USAGE_TELEPHONY_ALTERNATE_FUNCTION  = $29;
  HID_USAGE_TELEPHONY_LINE                = $2A;
  HID_USAGE_TELEPHONY_SPEAKER_PHONE       = $2B;
  HID_USAGE_TELEPHONY_CONFERENCE          = $2C;
  HID_USAGE_TELEPHONY_RING_ENABLE         = $2D;
  HID_USAGE_TELEPHONY_RING_SELECT         = $2E;
  HID_USAGE_TELEPHONY_PHONE_MUTE          = $2F;
  HID_USAGE_TELEPHONY_CALLER_ID           = $30;
  HID_USAGE_TELEPHONY_SEND                = $31;

  HID_USAGE_TELEPHONY_SPEED_DIAL          = $50;
  HID_USAGE_TELEPHONY_STORE_NUMBER        = $51;
  HID_USAGE_TELEPHONY_RECALL_NUMBER       = $52;
  HID_USAGE_TELEPHONY_PHONE_DIRECTORY     = $53;

  HID_USAGE_TELEPHONY_VOICE_MAIL          = $70;
  HID_USAGE_TELEPHONY_SCREEN_CALLS        = $71;
  HID_USAGE_TELEPHONY_DO_NOT_DISTURB      = $72;
  HID_USAGE_TELEPHONY_MESSAGE             = $73;
  HID_USAGE_TELEPHONY_ANSWER_ON_OFF       = $74;

  HID_USAGE_TELEPHONY_INSIDE_DIAL_TONE    = $90;
  HID_USAGE_TELEPHONY_OUTSIDE_DIAL_TONE   = $91;
  HID_USAGE_TELEPHONY_INSIDE_RING_TONE    = $92;
  HID_USAGE_TELEPHONY_OUTSIDE_RING_TONE   = $93;
  HID_USAGE_TELEPHONY_PRIORITY_RING_TONE  = $94;
  HID_USAGE_TELEPHONY_INSIDE_RINGBACK     = $95;
  HID_USAGE_TELEPHONY_PRIORITY_RINGBACK   = $96;
  HID_USAGE_TELEPHONY_LINE_BUSY_TONE      = $97;
  HID_USAGE_TELEPHONY_REORDER_TONE        = $98;
  HID_USAGE_TELEPHONY_CALL_WAITING_TONE   = $99;
  HID_USAGE_TELEPHONY_CONFIRMATION_TONE_1 = $9A;
  HID_USAGE_TELEPHONY_CONFIRMATION_TONE_2 = $9B;
  HID_USAGE_TELEPHONY_TONES_OFF           = $9C;
  HID_USAGE_TELEPHONY_OUTSIDE_RINGBACK    = $9D;
  HID_USAGE_TELEPHONY_RINGER              = $9E;

  HID_USAGE_TELEPHONY_KEY_0               = $B0;
  HID_USAGE_TELEPHONY_KEY_1               = $B1;
  HID_USAGE_TELEPHONY_KEY_2               = $B2;
  HID_USAGE_TELEPHONY_KEY_3               = $B3;
  HID_USAGE_TELEPHONY_KEY_4               = $B4;
  HID_USAGE_TELEPHONY_KEY_5               = $B5;
  HID_USAGE_TELEPHONY_KEY_6               = $B6;
  HID_USAGE_TELEPHONY_KEY_7               = $B7;
  HID_USAGE_TELEPHONY_KEY_8               = $B8;
  HID_USAGE_TELEPHONY_KEY_9               = $B9;
  HID_USAGE_TELEPHONY_KEY_STAR            = $BA;
  HID_USAGE_TELEPHONY_KEY_POUND           = $BB;
  HID_USAGE_TELEPHONY_KEY_A               = $BC;
  HID_USAGE_TELEPHONY_KEY_B               = $BD;
  HID_USAGE_TELEPHONY_KEY_C               = $BE;
  HID_USAGE_TELEPHONY_KEY_D               = $BF;

  // (rom) $C0 to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  // Consumer Page (0x0C)
  //
  HID_USAGE_CONSUMER_UNDEFINED                        = $000;
  HID_USAGE_CONSUMER_CONSUMER_CONTROL                 = $001;
  HID_USAGE_CONSUMER_NUMERIC_KEY_PAD                  = $002;
  HID_USAGE_CONSUMER_PROGRAMMABLE_BUTTONS             = $003;
  HID_USAGE_CONSUMER_MICROPHONE                       = $004;
  HID_USAGE_CONSUMER_HEADPHONE                        = $005;
  HID_USAGE_CONSUMER_GRAPHIC_EQUALIZER                = $006;

  HID_USAGE_CONSUMER_PLUS_10                          = $020;
  HID_USAGE_CONSUMER_PLUS_100                         = $021;
  HID_USAGE_CONSUMER_AM_PM                            = $022;

  HID_USAGE_CONSUMER_POWER                            = $030;
  HID_USAGE_CONSUMER_RESET                            = $031;
  HID_USAGE_CONSUMER_SLEEP                            = $032;
  HID_USAGE_CONSUMER_SLEEP_AFTER                      = $033;
  HID_USAGE_CONSUMER_SLEEP_MODE                       = $034;
  HID_USAGE_CONSUMER_ILLUMINATION                     = $035;
  HID_USAGE_CONSUMER_FUNCTION_BUTTONS                 = $036;

  HID_USAGE_CONSUMER_MENU                             = $040;
  HID_USAGE_CONSUMER_MENU_PICK                        = $041;
  HID_USAGE_CONSUMER_MENU_UP                          = $042;
  HID_USAGE_CONSUMER_MENU_DOWN                        = $043;
  HID_USAGE_CONSUMER_MENU_LEFT                        = $044;
  HID_USAGE_CONSUMER_MENU_RIGHT                       = $045;
  HID_USAGE_CONSUMER_MENU_ESCAPE                      = $046;
  HID_USAGE_CONSUMER_MENU_VALUE_INCREASE              = $047;
  HID_USAGE_CONSUMER_MENU_VALUE_DECREASE              = $048;

  HID_USAGE_CONSUMER_DATA_ON_SCREEN                   = $060;
  HID_USAGE_CONSUMER_CLOSED_CAPTION                   = $061;
  HID_USAGE_CONSUMER_CLOSED_CAPTION_SELECT            = $062;
  HID_USAGE_CONSUMER_VCR_TV                           = $063;
  HID_USAGE_CONSUMER_BROADCAST_MODE                   = $064;
  HID_USAGE_CONSUMER_SNAPSHOT                         = $065;
  HID_USAGE_CONSUMER_STILL                            = $066;

  HID_USAGE_CONSUMER_SELECTION                        = $080;
  HID_USAGE_CONSUMER_ASSIGN_SELECTION                 = $081;
  HID_USAGE_CONSUMER_MODE_STEP                        = $082;
  HID_USAGE_CONSUMER_RECALL_LAST                      = $083;
  HID_USAGE_CONSUMER_ENTER_CHANNEL                    = $084;
  HID_USAGE_CONSUMER_ORDER_MOVIE                      = $085;
  HID_USAGE_CONSUMER_CHANNEL                          = $086;
  HID_USAGE_CONSUMER_MEDIA_SELECTION                  = $087;
  HID_USAGE_CONSUMER_MEDIA_SELECT_COMPUTER            = $088;
  HID_USAGE_CONSUMER_MEDIA_SELECT_TV                  = $089;
  HID_USAGE_CONSUMER_MEDIA_SELECT_WWW                 = $08A;
  HID_USAGE_CONSUMER_MEDIA_SELECT_DVD                 = $08B;
  HID_USAGE_CONSUMER_MEDIA_SELECT_TELEPHONE           = $08C;
  HID_USAGE_CONSUMER_MEDIA_SELECT_PROGRAM_GUIDE       = $08D;
  HID_USAGE_CONSUMER_MEDIA_SELECT_VIDEO_PHONE         = $08E;
  HID_USAGE_CONSUMER_MEDIA_SELECT_GAMES               = $08F;
  HID_USAGE_CONSUMER_MEDIA_SELECT_MESSAGES            = $090;
  HID_USAGE_CONSUMER_MEDIA_SELECT_CD                  = $091;
  HID_USAGE_CONSUMER_MEDIA_SELECT_VCR                 = $092;
  HID_USAGE_CONSUMER_MEDIA_SELECT_TUNER               = $093;
  HID_USAGE_CONSUMER_QUIT                             = $094;
  HID_USAGE_CONSUMER_HELP                             = $095;
  HID_USAGE_CONSUMER_MEDIA_SELECT_TAPE                = $096;
  HID_USAGE_CONSUMER_MEDIA_SELECT_CABLE               = $097;
  HID_USAGE_CONSUMER_MEDIA_SELECT_SATELLITE           = $098;
  HID_USAGE_CONSUMER_MEDIA_SELECT_SECURITY            = $099;
  HID_USAGE_CONSUMER_MEDIA_SELECT_HOME                = $09A;
  HID_USAGE_CONSUMER_MEDIA_SELECT_CALL                = $09B;
  HID_USAGE_CONSUMER_CHANNEL_INCREMENT                = $09C;
  HID_USAGE_CONSUMER_CHANNEL_DECREMENT                = $09D;
  HID_USAGE_CONSUMER_MEDIA_SELECT_SAP                 = $09E;
  HID_USAGE_CONSUMER_RESERVED                         = $09F;
  HID_USAGE_CONSUMER_VCR_PLUS                         = $0A0;
  HID_USAGE_CONSUMER_ONCE                             = $0A1;
  HID_USAGE_CONSUMER_DAILY                            = $0A2;
  HID_USAGE_CONSUMER_WEEKLY                           = $0A3;
  HID_USAGE_CONSUMER_MONTHLY                          = $0A4;

  HID_USAGE_CONSUMER_PLAY                             = $0B0;
  HID_USAGE_CONSUMER_PAUSE                            = $0B1;
  HID_USAGE_CONSUMER_RECORD                           = $0B2;
  HID_USAGE_CONSUMER_FAST_FORWARD                     = $0B3;
  HID_USAGE_CONSUMER_REWIND                           = $0B4;
  HID_USAGE_CONSUMER_SCAN_NEXT_TRACK                  = $0B5;
  HID_USAGE_CONSUMER_SCAN_PREV_TRACK                  = $0B6;
  HID_USAGE_CONSUMER_STOP                             = $0B7;
  HID_USAGE_CONSUMER_EJECT                            = $0B8;
  HID_USAGE_CONSUMER_RANDOM_PLAY                      = $0B9;
  HID_USAGE_CONSUMER_SELECT_DISC                      = $0BA;
  HID_USAGE_CONSUMER_ENTER_DISC                       = $0BB;
  HID_USAGE_CONSUMER_REPEAT                           = $0BC;
  HID_USAGE_CONSUMER_TRACKING                         = $0BD;
  HID_USAGE_CONSUMER_TRACK_NORMAL                     = $0BE;
  HID_USAGE_CONSUMER_SLOW_TRACKING                    = $0BF;
  HID_USAGE_CONSUMER_FRAME_FORWARD                    = $0C0;
  HID_USAGE_CONSUMER_FRAME_BACK                       = $0C1;
  HID_USAGE_CONSUMER_MARK                             = $0C2;
  HID_USAGE_CONSUMER_CLEAR_MARK                       = $0C3;
  HID_USAGE_CONSUMER_REPEAT_FROM_MARK                 = $0C4;
  HID_USAGE_CONSUMER_RETURN_TO_MARK                   = $0C5;
  HID_USAGE_CONSUMER_SEARCH_MARK_FORWARD              = $0C6;
  HID_USAGE_CONSUMER_SEARCK_MARK_BACKWARDS            = $0C7;
  HID_USAGE_CONSUMER_COUNTER_RESET                    = $0C8;
  HID_USAGE_CONSUMER_SHOW_COUNTER                     = $0C9;
  HID_USAGE_CONSUMER_TRACKING_INCREMENT               = $0CA;
  HID_USAGE_CONSUMER_TRACKING_DECREMENT               = $0CB;
  HID_USAGE_CONSUMER_STOP_EJECT                       = $0CC;
  HID_USAGE_CONSUMER_PLAY_PAUSE                       = $0CD;
  HID_USAGE_CONSUMER_PLAY_SKIP                        = $0CE;

  HID_USAGE_CONSUMER_VOLUME                           = $0E0;
  HID_USAGE_CONSUMER_BALANCE                          = $0E1;
  HID_USAGE_CONSUMER_MUTE                             = $0E2;
  HID_USAGE_CONSUMER_BASS                             = $0E3;
  HID_USAGE_CONSUMER_TREBLE                           = $0E4;
  HID_USAGE_CONSUMER_BASS_BOOST                       = $0E5;
  HID_USAGE_CONSUMER_SURROUND_MODE                    = $0E6;
  HID_USAGE_CONSUMER_LOUDNESS                         = $0E7;
  HID_USAGE_CONSUMER_MPX                              = $0E8;
  HID_USAGE_CONSUMER_VOLUME_INCREMENT                 = $0E9;
  HID_USAGE_CONSUMER_VOLUME_DECREMENT                 = $0EA;

  HID_USAGE_CONSUMER_SPEED_SELECT                     = $0F0;
  HID_USAGE_CONSUMER_PLAYBACK_SPEED                   = $0F1;
  HID_USAGE_CONSUMER_STANDARD_PLAY                    = $0F2;
  HID_USAGE_CONSUMER_LONG_PLAY                        = $0F3;
  HID_USAGE_CONSUMER_EXTENDED_PLAY                    = $0F4;
  HID_USAGE_CONSUMER_SLOW                             = $0F5;

  HID_USAGE_CONSUMER_FAN_ENABLE                       = $100;
  HID_USAGE_CONSUMER_FAN_SPEED                        = $101;
  HID_USAGE_CONSUMER_LIGHT_ENABLE                     = $102;
  HID_USAGE_CONSUMER_LIGHT_ILLUMINATION_LEVEL         = $103;
  HID_USAGE_CONSUMER_CLIMATE_CONTROL_ENABLE           = $104;
  HID_USAGE_CONSUMER_ROOM_TEMPERATURE                 = $105;
  HID_USAGE_CONSUMER_SECURITY_ENABLE                  = $106;
  HID_USAGE_CONSUMER_FIRE_ALARM                       = $107;
  HID_USAGE_CONSUMER_POLICE_ALARM                     = $108;
  HID_USAGE_CONSUMER_PROXIMITY                        = $109;
  HID_USAGE_CONSUMER_MOTION                           = $10A;
  HID_USAGE_CONSUMER_DURESS_ALARM                     = $10B;
  HID_USAGE_CONSUMER_HOLDUP_ALARM                     = $10C;
  HID_USAGE_CONSUMER_MEDICAL_ALARM                    = $10D;

  HID_USAGE_CONSUMER_BALANCE_RIGHT                    = $150;
  HID_USAGE_CONSUMER_BALANCE_LEFT                     = $151;
  HID_USAGE_CONSUMER_BASS_INCREMENT                   = $152;
  HID_USAGE_CONSUMER_BASS_DECREMENT                   = $153;
  HID_USAGE_CONSUMER_TREBLE_INCREMENT                 = $154;
  HID_USAGE_CONSUMER_TREBLE_DECREMENT                 = $155;

  HID_USAGE_CONSUMER_SPEAKER_SYSTEM                   = $160;
  HID_USAGE_CONSUMER_CHANNEL_LEFT                     = $161;
  HID_USAGE_CONSUMER_CHANNEL_RIGHT                    = $162;
  HID_USAGE_CONSUMER_CHANNEL_CENTER                   = $163;
  HID_USAGE_CONSUMER_CHANNEL_FRONT                    = $164;
  HID_USAGE_CONSUMER_CHANNEL_CENTER_FRONT             = $165;
  HID_USAGE_CONSUMER_CHANNEL_SIDE                     = $166;
  HID_USAGE_CONSUMER_CHANNEL_SURROUND                 = $167;
  HID_USAGE_CONSUMER_CHANNEL_LOW_FREQ_ENH             = $168;
  HID_USAGE_CONSUMER_CHANNEL_TOP                      = $169;
  HID_USAGE_CONSUMER_CHANNEL_UNKNOWN                  = $16A;

  HID_USAGE_CONSUMER_SUB_CHANNEL                      = $170;
  HID_USAGE_CONSUMER_SUB_CHANNEL_INCREMENT            = $171;
  HID_USAGE_CONSUMER_SUB_CHANNEL_DECREMENT            = $172;
  HID_USAGE_CONSUMER_ALTERNATE_AUDIO_INCREMENT        = $173;
  HID_USAGE_CONSUMER_ALTERNATE_AUDIO_DECREMENT        = $174;

  HID_USAGE_CONSUMER_APP_LAUNCH_BUTTONS               = $180;
  HID_USAGE_CONSUMER_AL_LAUNCH_BUTTON_CONFIG_TOOL     = $181;
  HID_USAGE_CONSUMER_AL_PROG_BUTTON_CONFIG            = $182;
  HID_USAGE_CONSUMER_AL_CONSUMER_CONTROL_CONFIG       = $183;
  HID_USAGE_CONSUMER_AL_WORD_PROCESSOR                = $184;
  HID_USAGE_CONSUMER_AL_TEXT_EDITOR                   = $185;
  HID_USAGE_CONSUMER_AL_SPREADSHEET                   = $186;
  HID_USAGE_CONSUMER_AL_GRAPHICS_EDITOR               = $187;
  HID_USAGE_CONSUMER_AL_PRESENTATION_APP              = $188;
  HID_USAGE_CONSUMER_AL_DATABASE_APP                  = $189;
  HID_USAGE_CONSUMER_AL_EMAIL_READER                  = $18A;
  HID_USAGE_CONSUMER_AL_NEWSREADER                    = $18B;
  HID_USAGE_CONSUMER_AL_VOICEMAIL                     = $18C;
  HID_USAGE_CONSUMER_AL_CONTACTS_ADDESSBOOK           = $18D;
  HID_USAGE_CONSUMER_AL_CALENDAR_SCHEDULE             = $18E;
  HID_USAGE_CONSUMER_AL_TASK_PROJECT_MANAGER          = $18F;
  HID_USAGE_CONSUMER_AL_LOG_JOURNAL_TIMECARD          = $190;
  HID_USAGE_CONSUMER_AL_CHECKBOOK_FINANCE             = $191;
  HID_USAGE_CONSUMER_AL_CALCULATOR                    = $192;
  HID_USAGE_CONSUMER_AL_AV_CAPTURE_PLAYBACK           = $193;
  HID_USAGE_CONSUMER_AL_LOCAL_MACHINE_BROWSER         = $194;
  HID_USAGE_CONSUMER_AL_LAN_WAN_BROWSER               = $195;
  HID_USAGE_CONSUMER_AL_INTERNET_BROWSER              = $196;
  HID_USAGE_CONSUMER_AL_REMOTE_NETWORKING_ISP_CONNECT = $197;
  HID_USAGE_CONSUMER_AL_NETWORK_CONFERENCE            = $198;
  HID_USAGE_CONSUMER_AL_NETWORK_CHAT                  = $199;
  HID_USAGE_CONSUMER_AL_TELEPHONY_DIALER              = $19A;
  HID_USAGE_CONSUMER_AL_LOGON                         = $19B;
  HID_USAGE_CONSUMER_AL_LOGOFF                        = $19C;
  HID_USAGE_CONSUMER_AL_LOGON_LOGOFF                  = $19D;
  HID_USAGE_CONSUMER_AL_TERMINAL_LOCK_SCREENSAVER     = $19E;
  HID_USAGE_CONSUMER_AL_CONTROL_PANEL                 = $19F;
  HID_USAGE_CONSUMER_AL_COMMAND_LINE_PROCESSOR_RUN    = $1A0;
  HID_USAGE_CONSUMER_AL_PROCESS_TASK_MANAGER          = $1A1;
  HID_USAGE_CONSUMER_AL_SELECT_TASK_APP               = $1A2;
  HID_USAGE_CONSUMER_AL_NEXT_TASK_APP                 = $1A3;
  HID_USAGE_CONSUMER_AL_PREV_TASK_APP                 = $1A4;
  HID_USAGE_CONSUMER_AL_PREEMPTIVE_HALT_TASK_APP      = $1A5;
  HID_USAGE_CONSUMER_AL_INTEGRATED_HELP_CENTER        = $1A6;
  HID_USAGE_CONSUMER_AL_DOCUMENTS                     = $1A7;
  HID_USAGE_CONSUMER_AL_THESAURUS                     = $1A8;
  HID_USAGE_CONSUMER_AL_DICTIONARY                    = $1A9;
  HID_USAGE_CONSUMER_AL_DESKTOP                       = $1AA;
  HID_USAGE_CONSUMER_AL_SPELL_CHECK                   = $1AB;
  HID_USAGE_CONSUMER_AL_GRAMMAR_CHECK                 = $1AC;
  HID_USAGE_CONSUMER_AL_WIRELESS_STATUS               = $1AD;
  HID_USAGE_CONSUMER_AL_KEYBOARD_LAYOUT               = $1AE;
  HID_USAGE_CONSUMER_AL_VIRUS_PROTECTION              = $1AF;
  HID_USAGE_CONSUMER_AL_ENCRYPTION                    = $1B0;
  HID_USAGE_CONSUMER_AL_SCREENSAVER                   = $1B1;
  HID_USAGE_CONSUMER_AL_ALARMS                        = $1B2;
  HID_USAGE_CONSUMER_AL_CLOCK                         = $1B3;
  HID_USAGE_CONSUMER_AL_FILE_BROWSER                  = $1B4;
  HID_USAGE_CONSUMER_AL_POWER_STATUS                  = $1B5;

  HID_USAGE_CONSUMER_GENERIC_GUI_APP_CONTROLS         = $200;
  HID_USAGE_CONSUMER_AC_NEW                           = $201;
  HID_USAGE_CONSUMER_AC_OPEN                          = $202;
  HID_USAGE_CONSUMER_AC_CLOSE                         = $203;
  HID_USAGE_CONSUMER_AC_EXIT                          = $204;
  HID_USAGE_CONSUMER_AC_MAXIMIZE                      = $205;
  HID_USAGE_CONSUMER_AC_MINIMIZE                      = $206;
  HID_USAGE_CONSUMER_AC_SAVE                          = $207;
  HID_USAGE_CONSUMER_AC_PRINT                         = $208;
  HID_USAGE_CONSUMER_AC_PROPERTIES                    = $209;

  HID_USAGE_CONSUMER_AC_UNDO                          = $21A;
  HID_USAGE_CONSUMER_AC_COPY                          = $21B;
  HID_USAGE_CONSUMER_AC_CUT                           = $21C;
  HID_USAGE_CONSUMER_AC_PASTE                         = $21D;
  HID_USAGE_CONSUMER_AC_SELECT_ALL                    = $21E;
  HID_USAGE_CONSUMER_AC_FIND                          = $21F;
  HID_USAGE_CONSUMER_AC_FIND_AND_REPLACE              = $220;
  HID_USAGE_CONSUMER_AC_SEARCH                        = $221;
  HID_USAGE_CONSUMER_AC_GO_TO                         = $222;
  HID_USAGE_CONSUMER_AC_HOME                          = $223;
  HID_USAGE_CONSUMER_AC_BACK                          = $224;
  HID_USAGE_CONSUMER_AC_FORWARD                       = $225;
  HID_USAGE_CONSUMER_AC_STOP                          = $226;
  HID_USAGE_CONSUMER_AC_REFRESH                       = $227;
  HID_USAGE_CONSUMER_AC_PREV_LINK                     = $228;
  HID_USAGE_CONSUMER_AC_NEXT_LINK                     = $229;
  HID_USAGE_CONSUMER_AC_BOOKMARKS                     = $22A;
  HID_USAGE_CONSUMER_AC_HISTORY                       = $22B;
  HID_USAGE_CONSUMER_AC_SUBSCRIPTIONS                 = $22C;
  HID_USAGE_CONSUMER_AC_ZOOM_IN                       = $22D;
  HID_USAGE_CONSUMER_AC_ZOOM_OUT                      = $22E;
  HID_USAGE_CONSUMER_AC_ZOOM                          = $22F;
  HID_USAGE_CONSUMER_AC_FULL_SCREEN_VIEW              = $230;
  HID_USAGE_CONSUMER_AC_NORMAL_VIEW                   = $231;
  HID_USAGE_CONSUMER_AC_VIEW_TOGGLE                   = $232;
  HID_USAGE_CONSUMER_AC_SCROLL_UP                     = $233;
  HID_USAGE_CONSUMER_AC_SCROLL_DOWN                   = $234;
  HID_USAGE_CONSUMER_AC_SCROLL                        = $235;
  HID_USAGE_CONSUMER_AC_PAN_LEFT                      = $236;
  HID_USAGE_CONSUMER_AC_PAN_RIGHT                     = $237;
  HID_USAGE_CONSUMER_AC_PAN                           = $238;
  HID_USAGE_CONSUMER_AC_NEW_WINDOW                    = $239;
  HID_USAGE_CONSUMER_AC_TILE_HORIZONTALLY             = $23A;
  HID_USAGE_CONSUMER_AC_TILE_VERTICALLY               = $23B;
  HID_USAGE_CONSUMER_AC_FORMAT                        = $23C;
  HID_USAGE_CONSUMER_AC_EDIT                          = $23D;
  HID_USAGE_CONSUMER_AC_BOLD                          = $23E;
  HID_USAGE_CONSUMER_AC_ITALICS                       = $23F;
  HID_USAGE_CONSUMER_AC_UNDERLINE                     = $240;
  HID_USAGE_CONSUMER_AC_STRIKETHROUGH                 = $241;
  HID_USAGE_CONSUMER_AC_SUBSCRIPT                     = $242;
  HID_USAGE_CONSUMER_AC_SUPERSCRIPT                   = $243;
  HID_USAGE_CONSUMER_AC_ALL_CAPS                      = $244;
  HID_USAGE_CONSUMER_AC_ROTATE                        = $245;
  HID_USAGE_CONSUMER_AC_RESIZE                        = $246;
  HID_USAGE_CONSUMER_AC_FLIP_HORIZONTAL               = $247;
  HID_USAGE_CONSUMER_AC_FLIP_VERTICAL                 = $248;
  HID_USAGE_CONSUMER_AC_MIRROR_HORIZONTAL             = $249;
  HID_USAGE_CONSUMER_AC_MIRROR_VERTICAL               = $24A;
  HID_USAGE_CONSUMER_AC_FONT_SELECT                   = $24B;
  HID_USAGE_CONSUMER_AC_FONT_COLOR                    = $24C;
  HID_USAGE_CONSUMER_AC_FONT_SIZE                     = $24D;
  HID_USAGE_CONSUMER_AC_JUSTIFY_LEFT                  = $24E;
  HID_USAGE_CONSUMER_AC_JUSTIFY_CENTER_H              = $24F;
  HID_USAGE_CONSUMER_AC_JUSTIFY_RIGHT                 = $250;
  HID_USAGE_CONSUMER_AC_JUSTIFY_BLOCK_H               = $251;
  HID_USAGE_CONSUMER_AC_JUSTIFY_TOP                   = $252;
  HID_USAGE_CONSUMER_AC_JUSTIFY_CENTER_V              = $253;
  HID_USAGE_CONSUMER_AC_JUSTIFY_BOTTOM                = $254;
  HID_USAGE_CONSUMER_AC_JUSTIFY_BLOCK_V               = $255;
  HID_USAGE_CONSUMER_AC_INDENT_DECREASE               = $256;
  HID_USAGE_CONSUMER_AC_INDENT_INCREASE               = $257;
  HID_USAGE_CONSUMER_AC_NUMBERED_LIST                 = $258;
  HID_USAGE_CONSUMER_AC_RESTART_NUMBERING             = $259;
  HID_USAGE_CONSUMER_AC_BULLETED_LIST                 = $25A;
  HID_USAGE_CONSUMER_AC_PROMOTE                       = $25B;
  HID_USAGE_CONSUMER_AC_DEMOTE                        = $25C;
  HID_USAGE_CONSUMER_AC_YES                           = $25D;
  HID_USAGE_CONSUMER_AC_NO                            = $25E;
  HID_USAGE_CONSUMER_AC_CANCEL                        = $25F;
  HID_USAGE_CONSUMER_AC_CATALOG                       = $260;
  HID_USAGE_CONSUMER_AC_BUY_CHECKOUT                  = $261;
  HID_USAGE_CONSUMER_AC_ADD_TO_CART                   = $262;
  HID_USAGE_CONSUMER_AC_EXPAND                        = $263;
  HID_USAGE_CONSUMER_AC_EXPAND_ALL                    = $264;
  HID_USAGE_CONSUMER_AC_COLLAPSE                      = $265;
  HID_USAGE_CONSUMER_AC_COLLAPSE_ALL                  = $266;
  HID_USAGE_CONSUMER_AC_PRINT_PREVIEW                 = $267;
  HID_USAGE_CONSUMER_AC_PASTE_SPECIAL                 = $268;
  HID_USAGE_CONSUMER_AC_INSERT_MODE                   = $269;
  HID_USAGE_CONSUMER_AC_DELETE                        = $26A;
  HID_USAGE_CONSUMER_AC_LOCK                          = $26B;
  HID_USAGE_CONSUMER_AC_UNLOCK                        = $26C;
  HID_USAGE_CONSUMER_AC_PROTECT                       = $26D;
  HID_USAGE_CONSUMER_AC_UNPROTECT                     = $26E;
  HID_USAGE_CONSUMER_AC_ATTACH_COMMENT                = $26F;
  HID_USAGE_CONSUMER_AC_DELETE_COMMENT                = $270;
  HID_USAGE_CONSUMER_AC_VIEW_COMMENT                  = $271;
  HID_USAGE_CONSUMER_AC_SELECT_WORD                   = $272;
  HID_USAGE_CONSUMER_AC_SELECT_SENTENCE               = $273;
  HID_USAGE_CONSUMER_AC_SELECT_PARAGRAPH              = $274;
  HID_USAGE_CONSUMER_AC_SELECT_COLUMN                 = $275;
  HID_USAGE_CONSUMER_AC_SELECT_ROW                    = $276;
  HID_USAGE_CONSUMER_AC_SELECT_TABLE                  = $277;
  HID_USAGE_CONSUMER_AC_SELECT_OBJECT                 = $278;
  HID_USAGE_CONSUMER_AC_REDO_REPEAT                   = $279;
  HID_USAGE_CONSUMER_AC_SORT                          = $27A;
  HID_USAGE_CONSUMER_AC_SORT_ASCENDING                = $27B;
  HID_USAGE_CONSUMER_AC_SORT_DESCENDING               = $27C;
  HID_USAGE_CONSUMER_AC_FILTER                        = $27D;
  HID_USAGE_CONSUMER_AC_SET_CLOCK                     = $27E;
  HID_USAGE_CONSUMER_AC_VIEW_CLOCK                    = $27F;
  HID_USAGE_CONSUMER_AC_SELECT_TIME_ZONE              = $280;
  HID_USAGE_CONSUMER_AC_EDIT_TIME_ZONES               = $281;
  HID_USAGE_CONSUMER_AC_SET_ALARM                     = $282;
  HID_USAGE_CONSUMER_AC_CLEAR_ALARM                   = $283;
  HID_USAGE_CONSUMER_AC_SNOOZE_ALARM                  = $284;
  HID_USAGE_CONSUMER_AC_RESET_ALARM                   = $285;
  HID_USAGE_CONSUMER_AC_SYNCHRONIZE                   = $286;
  HID_USAGE_CONSUMER_AC_SEND_RECEIVE                  = $287;
  HID_USAGE_CONSUMER_AC_SEND_TO                       = $288;
  HID_USAGE_CONSUMER_AC_REPLY                         = $289;
  HID_USAGE_CONSUMER_AC_REPLY_ALL                     = $28A;
  HID_USAGE_CONSUMER_AC_FORWARD_MSG                   = $28B;
  HID_USAGE_CONSUMER_AC_SEND                          = $28C;
  HID_USAGE_CONSUMER_AC_ATTACH_FILE                   = $28D;
  HID_USAGE_CONSUMER_AC_UPLOAD                        = $28E;
  HID_USAGE_CONSUMER_AC_DOWNLOAD                      = $28F;
  HID_USAGE_CONSUMER_AC_SET_BORDERS                   = $290;
  HID_USAGE_CONSUMER_AC_INSERT_ROW                    = $291;
  HID_USAGE_CONSUMER_AC_INSERT_COLUMN                 = $292;
  HID_USAGE_CONSUMER_AC_INSERT_FILE                   = $293;
  HID_USAGE_CONSUMER_AC_INSERT_PICTURE                = $294;
  HID_USAGE_CONSUMER_AC_INSERT_OBJECT                 = $295;
  HID_USAGE_CONSUMER_AC_INSERT_SYMBOL                 = $296;
  HID_USAGE_CONSUMER_AC_SAVE_AND_CLOSE                = $297;
  HID_USAGE_CONSUMER_AC_RENAME                        = $298;
  HID_USAGE_CONSUMER_AC_MERGE                         = $299;
  HID_USAGE_CONSUMER_AC_SPLIT                         = $29A;
  HID_USAGE_CONSUMER_AC_DISTRIBUTE_HORIZONTALLY       = $29B;
  HID_USAGE_CONSUMER_AC_DISTRIBUTE_VERTICALLY         = $29C;

  // (rom) $29D to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  // Digitizer Page (0x0D)
  //
  HID_USAGE_DIGITIZER_UNDEFINED                    = $00;
  HID_USAGE_DIGITIZER_DIGITIZER                    = $01;
  HID_USAGE_DIGITIZER_PEN                          = $02;
  HID_USAGE_DIGITIZER_LIGHT_PEN                    = $03;
  HID_USAGE_DIGITIZER_TOUCH_SCREEN                 = $04;
  HID_USAGE_DIGITIZER_TOUCH_PAD                    = $05;
  HID_USAGE_DIGITIZER_WHITE_BOARD                  = $06;
  HID_USAGE_DIGITIZER_COORDINATE_MEASURING_MACHINE = $07;
  HID_USAGE_DIGITIZER_3D_DIGITIZER                 = $08;
  HID_USAGE_DIGITIZER_STEREO_PLOTTER               = $09;
  HID_USAGE_DIGITIZER_ARTICULATED_ARM              = $0A;
  HID_USAGE_DIGITIZER_ARMATURE                     = $0B;
  HID_USAGE_DIGITIZER_MULTIPLE_POINT_DIGITIZER     = $0C;
  HID_USAGE_DIGITIZER_FREE_SPACE_WAND              = $0D;

  HID_USAGE_DIGITIZER_STYLUS                       = $20;
  HID_USAGE_DIGITIZER_PUCK                         = $21;
  HID_USAGE_DIGITIZER_FINGER                       = $22;

  HID_USAGE_DIGITIZER_TIP_PRESSURE                 = $30;
  HID_USAGE_DIGITIZER_BARREL_PRESSURE              = $31;
  HID_USAGE_DIGITIZER_IN_RANGE                     = $32;
  HID_USAGE_DIGITIZER_TOUCH                        = $33;
  HID_USAGE_DIGITIZER_UNTOUCH                      = $34;
  HID_USAGE_DIGITIZER_TAP                          = $35;
  HID_USAGE_DIGITIZER_QUALITY                      = $36;
  HID_USAGE_DIGITIZER_DATA_VALID                   = $37;
  HID_USAGE_DIGITIZER_TRANSDUCER_INDEX             = $38;
  HID_USAGE_DIGITIZER_TABLET_FUNCTION_KEYS         = $39;
  HID_USAGE_DIGITIZER_PROGRAM_CHANGE_KEYS          = $3A;
  HID_USAGE_DIGITIZER_BATTERY_STRENGTH             = $3B;
  HID_USAGE_DIGITIZER_INVERT                       = $3C;
  HID_USAGE_DIGITIZER_X_TILT                       = $3D;
  HID_USAGE_DIGITIZER_Y_TILT                       = $3E;
  HID_USAGE_DIGITIZER_AZIMUTH                      = $3F;
  HID_USAGE_DIGITIZER_ALTITUDE                     = $40;
  HID_USAGE_DIGITIZER_TWIST                        = $41;
  HID_USAGE_DIGITIZER_TIP_SWITCH                   = $42;
  HID_USAGE_DIGITIZER_SECONDARY_TIP_SWITCH         = $43;
  HID_USAGE_DIGITIZER_BARREL_SWITCH                = $44;
  HID_USAGE_DIGITIZER_ERASER                       = $45;
  HID_USAGE_DIGITIZER_TABLET_PICK                  = $46;

  // (rom) $47 to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  // Physical Input Page (0x0F)
  //
  HID_USAGE_PID_UNDEFINED                        = $00;
  HID_USAGE_PID_PHYSICAL_INTERFACE_DEVICE        = $01;
                                                 
  HID_USAGE_PID_NORMAL                           = $20;
  HID_USAGE_PID_SET_EFFECT_REPORT                = $21;
  HID_USAGE_PID_EFFECT_BLOCK_INDEX               = $22;
  HID_USAGE_PID_PARAMETER_BLOCK_OFFSET           = $23;
  HID_USAGE_PID_ROM_FLAG                         = $24;
  HID_USAGE_PID_EFFECT_TYPE                      = $25;
  HID_USAGE_PID_ET_CONSTANT_FORCE                = $26;
  HID_USAGE_PID_ET_RAMP                          = $27;
  HID_USAGE_PID_ET_CUSTOM_FORCE_DATA             = $28;

  HID_USAGE_PID_ET_SQUARE                        = $30;
  HID_USAGE_PID_ET_SINE                          = $31;
  HID_USAGE_PID_ET_TRIANGLE                      = $32;
  HID_USAGE_PID_ET_SAWTOOTH_UP                   = $33;
  HID_USAGE_PID_ET_SAWTOOTH_DOWN                 = $34;

  HID_USAGE_PID_ET_SPRING                        = $40;
  HID_USAGE_PID_ET_DAMPER                        = $41;
  HID_USAGE_PID_ET_INERTIA                       = $42;
  HID_USAGE_PID_ET_FRICTION                      = $43;

  HID_USAGE_PID_DURATION                         = $50;
  HID_USAGE_PID_SAMPLE_PERIOD                    = $51;
  HID_USAGE_PID_GAIN                             = $52;
  HID_USAGE_PID_TRIGGER_BUTTON                   = $53;
  HID_USAGE_PID_TRIGGER_REPEAT_INTERVAL          = $54;
  HID_USAGE_PID_AXES_ENABLE                      = $55;
  HID_USAGE_PID_DIRECTION_ENABLE                 = $56;
  HID_USAGE_PID_DIRECTION                        = $57;
  HID_USAGE_PID_TYPE_SPECIFIC_BLOCK_OFFSET       = $58;
  HID_USAGE_PID_BLOCK_TYPE                       = $59;
  HID_USAGE_PID_SET_ENVELOPE_REPORT              = $5A;
  HID_USAGE_PID_ATTACK_LEVEL                     = $5B;
  HID_USAGE_PID_ATTACK_TIME                      = $5C;
  HID_USAGE_PID_FADE_LEVEL                       = $5D;
  HID_USAGE_PID_FADE_TIME                        = $5E;
  HID_USAGE_PID_SET_CONDITION_REPORT             = $5F;
  HID_USAGE_PID_CP_OFFSET                        = $60;
  HID_USAGE_PID_POSITIVE_COEFFICIENT             = $61;
  HID_USAGE_PID_NEGATIVE_COEFFICIENT             = $62;
  HID_USAGE_PID_POSITIVE_SATURATION              = $63;
  HID_USAGE_PID_NEGATIVE_SATURATION              = $64;
  HID_USAGE_PID_DEAD_BAND                        = $65;
  HID_USAGE_PID_DOWNLOAD_FORCE_SAMPLE            = $66;
  HID_USAGE_PID_ISOCH_CUSTOM_FORCE_ENABLE        = $67;
  HID_USAGE_PID_CUSTOM_FORCE_DATA_REPORT         = $68;
  HID_USAGE_PID_CUSTOM_FORCE_DATA                = $69;
  HID_USAGE_PID_CUSTOM_FORCE_VENDOR_DEFINED_DATA = $6A;
  HID_USAGE_PID_SET_CUSTOM_FORCE_REPORT          = $6B;
  HID_USAGE_PID_CUSTOM_FORCE_DATA_OFFSET         = $6C;
  HID_USAGE_PID_SAMPLE_COUNT                     = $6D;
  HID_USAGE_PID_SET_PERIODIC_REPORT              = $6E;
  HID_USAGE_PID_OFFSET                           = $6F;
  HID_USAGE_PID_MAGNITUDE                        = $70;
  HID_USAGE_PID_PHASE                            = $71;
  HID_USAGE_PID_PERIOD                           = $72;
  HID_USAGE_PID_SET_CONSTANT_FORCE_REPORT        = $73;
  HID_USAGE_PID_SET_RAMP_FORCE_REPORT            = $74;
  HID_USAGE_PID_RAMP_START                       = $75;
  HID_USAGE_PID_RAMP_END                         = $76;
  HID_USAGE_PID_EFFECT_OPERATION_REPORT          = $77;
  HID_USAGE_PID_EFFECT_OPERATION                 = $78;
  HID_USAGE_PID_OP_EFFECT_START                  = $79;
  HID_USAGE_PID_OP_EFFECT_START_SOLO             = $7A;
  HID_USAGE_PID_OP_EFFECT_STOP                   = $7B;
  HID_USAGE_PID_LOOP_COUNT                       = $7C;
  HID_USAGE_PID_DEVICE_GAIN_REPORT               = $7D;
  HID_USAGE_PID_DEVICE_GAIN                      = $7E;
  HID_USAGE_PID_PID_POOL_REPORT                  = $7F;
  HID_USAGE_PID_RAM_POOL_SIZE                    = $80;
  HID_USAGE_PID_ROM_POOL_SIZE                    = $81;
  HID_USAGE_PID_ROM_EFFECT_BLOCK_COUNT           = $82;
  HID_USAGE_PID_SIMULTANEOUS_EFFECTS_MAX         = $83;
  HID_USAGE_PID_POOL_ALIGNMENT                   = $84;
  HID_USAGE_PID_PID_POOL_MOVE_REPORT             = $85;
  HID_USAGE_PID_MOVE_SOURCE                      = $86;
  HID_USAGE_PID_MOVE_DESTINATION                 = $87;
  HID_USAGE_PID_MOVE_LENGTH                      = $88;
  HID_USAGE_PID_PID_BLOCK_LOAD_REPORT            = $89;

  HID_USAGE_PID_BLOCK_LOAD_STATUS                = $8B;
  HID_USAGE_PID_BLOCK_LOAD_SUCCESS               = $8C;
  HID_USAGE_PID_BLOCK_LOAD_FULL                  = $8D;
  HID_USAGE_PID_BLOCK_LOAD_ERROR                 = $8E;
  HID_USAGE_PID_BLOCK_HANDLE                     = $8F;
  HID_USAGE_PID_PID_BLOCK_FREE_REPORT            = $90;
  HID_USAGE_PID_TYPE_SPECIFIC_BLOCK_HANDLE       = $91;
  HID_USAGE_PID_PID_STATE_REPORT                 = $92;

  HID_USAGE_PID_EFFECT_PLAYING                   = $94;
  HID_USAGE_PID_PID_DEVICE_CONTROL_REPORT        = $95;
  HID_USAGE_PID_PID_DEVICE_CONTROL               = $96;
  HID_USAGE_PID_DC_ENABLE_ACTUATORS              = $97;
  HID_USAGE_PID_DC_DISABLE_ACTUATORS             = $98;
  HID_USAGE_PID_DC_STOP_ALL_EFFECTS              = $99;
  HID_USAGE_PID_DC_DEVICE_RESET                  = $9A;
  HID_USAGE_PID_DC_DEVICE_PAUSE                  = $9B;
  HID_USAGE_PID_DC_DEVICE_CONTINUE               = $9C;

  HID_USAGE_PID_DEVICE_PAUSED                    = $9F;
  HID_USAGE_PID_ACTUATORS_ENABLED                = $A0;

  HID_USAGE_PID_SAFETY_SWITCH                    = $A4;
  HID_USAGE_PID_ACTUATOR_OVERRIDE_SWITCH         = $A5;
  HID_USAGE_PID_ACTUATOR_POWER                   = $A6;
  HID_USAGE_PID_START_DELAY                      = $A7;
  HID_USAGE_PID_PARAMETER_BLOCK_SIZE             = $A8;
  HID_USAGE_PID_DEVICE_MANAGED_POOL              = $A9;
  HID_USAGE_PID_SHARED_PARAMETER_BLOCKS          = $AA;
  HID_USAGE_PID_CREATE_NEW_EFFECT_REPORT         = $AB;
  HID_USAGE_PID_RAM_POOL_AVAILABLE               = $AC;

  // (rom) $AD to $FFFF are reserved in "Device Class Definition for Physical Interface Devices 1.0" (pid1_01.pdf)

  //
  // Unicode Page (0x10)
  //
  // (rom) The Unicode Page directly maps to the two-octet form defined in the Unicode Standard

  //
  // Alphanumeric Display Page (0x14)
  //
  HID_USAGE_ALNUM_DISPLAY_UNDEFINED                    = $00;
  HID_USAGE_ALNUM_DISPLAY_ALPHANUMERIC_DISPLAY         = $01;

  HID_USAGE_ALNUM_DISPLAY_DISPLAY_ATTRIBUTES_REPORT    = $20;
  HID_USAGE_ALNUM_DISPLAY_ASCII_CHARSET                = $21;
  HID_USAGE_ALNUM_DISPLAY_DATA_READ_BACK               = $22;
  HID_USAGE_ALNUM_DISPLAY_FONT_READ_BACK               = $23;
  HID_USAGE_ALNUM_DISPLAY_DISPLAY_CONTROL_REPORT       = $24;
  HID_USAGE_ALNUM_DISPLAY_CLEAR_DISPLAY                = $25;
  HID_USAGE_ALNUM_DISPLAY_DISPLAY_ENABLE               = $26;
  HID_USAGE_ALNUM_DISPLAY_SCREEN_SAVER_DELAY           = $27;
  HID_USAGE_ALNUM_DISPLAY_SCREEN_SAVER_ENABLE          = $28;
  HID_USAGE_ALNUM_DISPLAY_VERTICAL_SCROLL              = $29;
  HID_USAGE_ALNUM_DISPLAY_HORIZONTAL_SCROLL            = $2A;
  HID_USAGE_ALNUM_DISPLAY_CHARACTER_REPORT             = $2B;
  HID_USAGE_ALNUM_DISPLAY_DISPLAY_DATA                 = $2C;
  HID_USAGE_ALNUM_DISPLAY_DISPLAY_STATUS               = $2D;
  HID_USAGE_ALNUM_DISPLAY_STAT_NOT_READY               = $2E;
  HID_USAGE_ALNUM_DISPLAY_STAT_READY                   = $2F;
  HID_USAGE_ALNUM_DISPLAY_ERR_NOT_A_LOADABLE_CHAR      = $30;
  HID_USAGE_ALNUM_DISPLAY_ERR_FONT_DATA_CANNOT_BE_READ = $31;
  HID_USAGE_ALNUM_DISPLAY_CURSOR_POSITION_REPORT       = $32;
  HID_USAGE_ALNUM_DISPLAY_ROW                          = $33;
  HID_USAGE_ALNUM_DISPLAY_COLUMN                       = $34;
  HID_USAGE_ALNUM_DISPLAY_ROWS                         = $35;
  HID_USAGE_ALNUM_DISPLAY_COLUMNS                      = $36;
  HID_USAGE_ALNUM_DISPLAY_CURSOR_PIXEL_POSITIONING     = $37;
  HID_USAGE_ALNUM_DISPLAY_CURSOR_MODE                  = $38;
  HID_USAGE_ALNUM_DISPLAY_CURSOR_ENABLE                = $39;
  HID_USAGE_ALNUM_DISPLAY_CURSOR_BLINK                 = $3A;
  HID_USAGE_ALNUM_DISPLAY_FONT_REPORT                  = $3B;
  HID_USAGE_ALNUM_DISPLAY_FONT_DATA                    = $3C;
  HID_USAGE_ALNUM_DISPLAY_CHAR_WIDTH                   = $3D;
  HID_USAGE_ALNUM_DISPLAY_CHAR_HEIGHT                  = $3E;
  HID_USAGE_ALNUM_DISPLAY_CHAR_SPACING_HORIZONTAL      = $3F;
  HID_USAGE_ALNUM_DISPLAY_CHAR_SPACING_VERTICAL        = $40;
  HID_USAGE_ALNUM_DISPLAY_UNICODE_CHARSET              = $41;
  HID_USAGE_ALNUM_DISPLAY_FONT_7_SEGMENT               = $42;
  HID_USAGE_ALNUM_DISPLAY_7_SEGMENT_DIRECT_MAP         = $43;
  HID_USAGE_ALNUM_DISPLAY_FONT_14_SEGMENT              = $44;
  HID_USAGE_ALNUM_DISPLAY_14_SEGMENT_DIRECT_MAP        = $45;
  HID_USAGE_ALNUM_DISPLAY_DISPLAY_BRIGHTNESS           = $46;
  HID_USAGE_ALNUM_DISPLAY_DISPLAY_CONTRAST             = $47;
  HID_USAGE_ALNUM_DISPLAY_CHAR_ATTRIBUTE               = $48;
  HID_USAGE_ALNUM_DISPLAY_ATTRIBUTE_READBACK           = $49;
  HID_USAGE_ALNUM_DISPLAY_ATTRIBUTE_DATA               = $4A;
  HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_ENHANCE            = $4B;
  HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_UNDERLINE          = $4C;
  HID_USAGE_ALNUM_DISPLAY_CHAR_ATTR_BLINK              = $4D;

  // (rom) $4E to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  // Medical Instrument Page (0x40)
  //
  HID_USAGE_MEDICAL_INSTRUMENT_UNDEFINED                    = $00;
  HID_USAGE_MEDICAL_INSTRUMENT_MEDICAL_ULTRASOUND           = $01;

  HID_USAGE_MEDICAL_INSTRUMENT_VCR_AQUISITION               = $20;
  HID_USAGE_MEDICAL_INSTRUMENT_FREEZE_THAW                  = $21;
  HID_USAGE_MEDICAL_INSTRUMENT_CLIP_STORE                   = $22;
  HID_USAGE_MEDICAL_INSTRUMENT_UPDATE                       = $23;
  HID_USAGE_MEDICAL_INSTRUMENT_NEXT                         = $24;
  HID_USAGE_MEDICAL_INSTRUMENT_SAVE                         = $25;
  HID_USAGE_MEDICAL_INSTRUMENT_PRINT                        = $26;
  HID_USAGE_MEDICAL_INSTRUMENT_MICROPHONE_ENABLE            = $27;

  HID_USAGE_MEDICAL_INSTRUMENT_CINE                         = $40;
  HID_USAGE_MEDICAL_INSTRUMENT_TRANSMIT_POWER               = $41;
  HID_USAGE_MEDICAL_INSTRUMENT_VOLUME                       = $42;
  HID_USAGE_MEDICAL_INSTRUMENT_FOCUS                        = $43;
  HID_USAGE_MEDICAL_INSTRUMENT_DEPTH                        = $44;

  HID_USAGE_MEDICAL_INSTRUMENT_SOFT_STEP_PRIMARY            = $60;
  HID_USAGE_MEDICAL_INSTRUMENT_SOFT_STEP_SECONDARY          = $61;

  HID_USAGE_MEDICAL_INSTRUMENT_DEPTH_GAIN_COMPENSATION      = $70;

  HID_USAGE_MEDICAL_INSTRUMENT_ZOOM_SELECT                  = $80;
  HID_USAGE_MEDICAL_INSTRUMENT_ZOOM_ADJUST                  = $81;
  HID_USAGE_MEDICAL_INSTRUMENT_SPECTRAL_DOPPLER_MODE_SELECT = $82;
  HID_USAGE_MEDICAL_INSTRUMENT_SPECTRAL_DOPPLER_ADJUST      = $83;
  HID_USAGE_MEDICAL_INSTRUMENT_COLOR_DOPPLER_MODE_SELECT    = $84;
  HID_USAGE_MEDICAL_INSTRUMENT_COLOR_DOPPLER_ADJUST         = $85;
  HID_USAGE_MEDICAL_INSTRUMENT_MOTION_MODE_SELECT           = $86;
  HID_USAGE_MEDICAL_INSTRUMENT_MOTION_MODE_ADJUST           = $87;
  HID_USAGE_MEDICAL_INSTRUMENT_2D_MODE_SELECT               = $88;
  HID_USAGE_MEDICAL_INSTRUMENT_2D_MODE_ADJUST               = $89;

  HID_USAGE_MEDICAL_INSTRUMENT_SOFT_CONTROL_SELECT          = $A0;
  HID_USAGE_MEDICAL_INSTRUMENT_SOFT_CONTROL_ADJUST          = $A1;

  // (rom) $A2 to $FFFF are reserved in "USB HID Usage Tables 1.11" (Hut1_11.pdf)

  //
  // USB Monitor Page (0x80)
  //
  HID_USAGE_MONITOR_RESERVED         = $00;
  HID_USAGE_MONITOR_MONITOR_CONTROL  = $01;
  HID_USAGE_MONITOR_EDID_INFORMATION = $02;
  HID_USAGE_MONITOR_VDIF_INFORMATION = $03;
  HID_USAGE_MONITOR_VESA_VERSION     = $04;

  //
  // Monitor Enumerated Values Page (0x81)
  //
  HID_USAGE_MONITOR_ENUM_VALUE_NO_VALUE = $00;

  // (rom) read "usbmon10.pdf" from USB IF for more info

  //
  // Monitor VESA Virtual Control Page (0x82)
  //
  HID_USAGE_MONITOR_VESA_BRIGHTNESS                       = $10;
  HID_USAGE_MONITOR_VESA_CONTRAST                         = $12;
  HID_USAGE_MONITOR_VESA_RED_VIDEO_GAIN                   = $16;
  HID_USAGE_MONITOR_VESA_GREEN_VIDEO_GAIN                 = $18;
  HID_USAGE_MONITOR_VESA_BLUE_VIDEO_GAIN                  = $1A;
  HID_USAGE_MONITOR_VESA_FOCUS                            = $1C;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_POS                   = $20;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_SIZE                  = $22;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_PINCUSHION            = $24;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_PINCUSHION_BALANCE    = $26;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_MISCONVERGENCE        = $28;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_LINEARITY             = $2A;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_LINEARITY_BALANCE     = $2C;
  HID_USAGE_MONITOR_VESA_VERTICAL_POS                     = $30;
  HID_USAGE_MONITOR_VESA_VERTICAL_SIZE                    = $32;
  HID_USAGE_MONITOR_VESA_VERTICAL_PINCUSHION              = $34;
  HID_USAGE_MONITOR_VESA_VERTICAL_PINCUSHION_BALANCE      = $36;
  HID_USAGE_MONITOR_VESA_VERTICAL_MISCONVERGENCE          = $38;
  HID_USAGE_MONITOR_VESA_VERTICAL_LINEARITY               = $3A;
  HID_USAGE_MONITOR_VESA_VERTICAL_LINEARITY_BALANCE       = $3C;
  HID_USAGE_MONITOR_VESA_PARALLELOGRAM_DISTORTION         = $40;
  HID_USAGE_MONITOR_VESA_TRAPEZOIDAL_DISTORTION           = $42;
  HID_USAGE_MONITOR_VESA_TILT                             = $44;
  HID_USAGE_MONITOR_VESA_TOP_CORNER_DISTORTION            = $46;
  HID_USAGE_MONITOR_VESA_TOP_CORNER_DISTORTION_BALANCE    = $48;
  HID_USAGE_MONITOR_VESA_BOTTOM_CORNER_DISTORTION         = $4A;
  HID_USAGE_MONITOR_VESA_BOTTOM_CORNER_DISTORTION_BALANCE = $4C;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_MOIRE                 = $56;
  HID_USAGE_MONITOR_VESA_VERTICAL_MOIRE                   = $58;
  HID_USAGE_MONITOR_VESA_RED_VIDEO_BLACK_LEVEL            = $6C;
  HID_USAGE_MONITOR_VESA_GREEN_VIDEO_BLACK_LEVEL          = $6E;
  HID_USAGE_MONITOR_VESA_BLUE_VIDEO_BLACK_LEVEL           = $70;
  HID_USAGE_MONITOR_VESA_INPUT_LEVEL_SELECT               = $5E;
  HID_USAGE_MONITOR_VESA_INPUT_SOURCE_SELECT              = $60;
  HID_USAGE_MONITOR_VESA_ON_SCREEN_DISPLAY                = $CA;
  HID_USAGE_MONITOR_VESA_STEREO_MODE                      = $D4;
  HID_USAGE_MONITOR_VESA_AUTO_SIZE_CENTER                 = $A2;
  HID_USAGE_MONITOR_VESA_POLARITY_HORIZONTAL_SYNC         = $A4;
  HID_USAGE_MONITOR_VESA_POLARITY_VERTICAL_SYNC           = $A6;
  HID_USAGE_MONITOR_VESA_SYNC_TYPE                        = $A8;
  HID_USAGE_MONITOR_VESA_SCREEN_ORIENTATION               = $AA;
  HID_USAGE_MONITOR_VESA_HORIZONTAL_FREQUENCY             = $AC;
  HID_USAGE_MONITOR_VESA_VERTICAL_FREQUENCY               = $AE;
  HID_USAGE_MONITOR_VESA_DEGAUSS                          = $01;
  HID_USAGE_MONITOR_VESA_SETTINGS                         = $B0;

  //
  // Monitor Reserved Page (0x83)
  //

  //
  // Power Device Page (0x84)
  //
  HID_USAGE_POWER_DEVICE_UNDEFINED              = $00;
  HID_USAGE_POWER_DEVICE_INAME                  = $01;
  HID_USAGE_POWER_DEVICE_PRESENT_STATUS         = $02;
  HID_USAGE_POWER_DEVICE_CHANGED_STATUS         = $03;
  HID_USAGE_POWER_DEVICE_UPS                    = $04;
  HID_USAGE_POWER_DEVICE_POWER_SUPPLY           = $05;

  HID_USAGE_POWER_DEVICE_BATTERY_SYSTEM         = $10;
  HID_USAGE_POWER_DEVICE_BATTERY_SYSTEM_ID      = $11;
  HID_USAGE_POWER_DEVICE_BATTERY                = $12;
  HID_USAGE_POWER_DEVICE_BATTERY_ID             = $13;
  HID_USAGE_POWER_DEVICE_CHARGER                = $14;
  HID_USAGE_POWER_DEVICE_CHARGER_ID             = $15;
  HID_USAGE_POWER_DEVICE_POWER_CONVERTER        = $16;
  HID_USAGE_POWER_DEVICE_POWER_CONVERTER_ID     = $17;
  HID_USAGE_POWER_DEVICE_OUTLET_SYSTEM          = $18;
  HID_USAGE_POWER_DEVICE_OUTLET_SYSTEM_ID       = $19;
  HID_USAGE_POWER_DEVICE_INPUT                  = $1A;
  HID_USAGE_POWER_DEVICE_INPUT_ID               = $1B;
  HID_USAGE_POWER_DEVICE_OUTPUT                 = $1C;
  HID_USAGE_POWER_DEVICE_OUTPUT_ID              = $1D;
  HID_USAGE_POWER_DEVICE_FLOW                   = $1E;
  HID_USAGE_POWER_DEVICE_FLOW_ID                = $1F;
  HID_USAGE_POWER_DEVICE_OUTLET                 = $20;
  HID_USAGE_POWER_DEVICE_OUTLET_ID              = $21;
  HID_USAGE_POWER_DEVICE_GANG                   = $22;
  HID_USAGE_POWER_DEVICE_GANG_ID                = $23;
  HID_USAGE_POWER_DEVICE_POWER_SUMMARY          = $24;
  HID_USAGE_POWER_DEVICE_POWER_SUMMARY_ID       = $25;

  HID_USAGE_POWER_DEVICE_VOLTAGE                = $30;
  HID_USAGE_POWER_DEVICE_CURRENT                = $31;
  HID_USAGE_POWER_DEVICE_FREQUENCY              = $32;
  HID_USAGE_POWER_DEVICE_APPARENT_POWER         = $33;
  HID_USAGE_POWER_DEVICE_ACTIVE_POWER           = $34;
  HID_USAGE_POWER_DEVICE_PERCENT_LOAD           = $35;
  HID_USAGE_POWER_DEVICE_TEMPERATURE            = $36;
  HID_USAGE_POWER_DEVICE_HUMIDITY               = $37;
  HID_USAGE_POWER_DEVICE_BAD_COUNT              = $38;

  HID_USAGE_POWER_DEVICE_CONFIG_VOLTAGE         = $40;
  HID_USAGE_POWER_DEVICE_CONFIG_CURRENT         = $41;
  HID_USAGE_POWER_DEVICE_CONFIG_FREQUENCY       = $42;
  HID_USAGE_POWER_DEVICE_CONFIG_APPARENT_POWER  = $43;
  HID_USAGE_POWER_DEVICE_CONFIG_ACTIVE_POWER    = $44;
  HID_USAGE_POWER_DEVICE_CONFIG_PERCENT_LOAD    = $45;
  HID_USAGE_POWER_DEVICE_CONFIG_TEMPERATURE     = $46;
  HID_USAGE_POWER_DEVICE_CONFIG_HUMIDITY        = $47;

  HID_USAGE_POWER_DEVICE_SWITCH_ON_CONTROL      = $50;
  HID_USAGE_POWER_DEVICE_SWITCH_OFF_CONTROL     = $51;
  HID_USAGE_POWER_DEVICE_TOGGLE_CONTROL         = $52;
  HID_USAGE_POWER_DEVICE_LOW_VOLTAGE_TRANSFER   = $53;
  HID_USAGE_POWER_DEVICE_HIGH_VOLTAGE_TRANSFER  = $54;
  HID_USAGE_POWER_DEVICE_DELAY_BEFORE_REBOOT    = $55;
  HID_USAGE_POWER_DEVICE_DELAY_BEFORE_STARTUP   = $56;
  HID_USAGE_POWER_DEVICE_DELAY_BEFORE_SHUTDOWN  = $57;
  HID_USAGE_POWER_DEVICE_TEST                   = $58;
  HID_USAGE_POWER_DEVICE_MODULE_RESET           = $59;
  HID_USAGE_POWER_DEVICE_AUDIBLE_ALARM_CONTROL  = $5A;

  HID_USAGE_POWER_DEVICE_PRESENT                = $60;
  HID_USAGE_POWER_DEVICE_GOOD                   = $61;
  HID_USAGE_POWER_DEVICE_INTERNAL_FAILURE       = $62;
  HID_USAGE_POWER_DEVICE_VOLTAGE_OUT_OF_RANGE   = $63;
  HID_USAGE_POWER_DEVICE_FREQUENCY_OUT_OF_RANGE = $64;
  HID_USAGE_POWER_DEVICE_OVERLOAD               = $65;
  HID_USAGE_POWER_DEVICE_OVERCHARGED            = $66;
  HID_USAGE_POWER_DEVICE_OVERTEMPERATURE        = $67;
  HID_USAGE_POWER_DEVICE_SHUTDOWN_REQUESTED     = $68;
  HID_USAGE_POWER_DEVICE_SHUTDOWN_IMMINENT      = $69;

  HID_USAGE_POWER_DEVICE_SWITCH_ON_OFF          = $6B;
  HID_USAGE_POWER_DEVICE_SWITCHABLE             = $6C;
  HID_USAGE_POWER_DEVICE_USED                   = $6D;
  HID_USAGE_POWER_DEVICE_BOOST                  = $6E;
  HID_USAGE_POWER_DEVICE_BUCK                   = $6F;
  HID_USAGE_POWER_DEVICE_INITIALIZED            = $70;
  HID_USAGE_POWER_DEVICE_TESTED                 = $71;
  HID_USAGE_POWER_DEVICE_AWAITING_POWER         = $72;
  HID_USAGE_POWER_DEVICE_COMMUNICATION_LOST     = $73;

  HID_USAGE_POWER_DEVICE_IMANUFACTURER          = $FD;
  HID_USAGE_POWER_DEVICE_IPRODUCT               = $FE;
  HID_USAGE_POWER_DEVICE_ISERIALNUMBER          = $FF;

  //
  // Battery System Page (0x85)
  //
  HID_USAGE_BATTERY_SYSTEM_UNDEFINED                      = $00;
  HID_USAGE_BATTERY_SYSTEM_SMB_BATTERY_MODE               = $01;
  HID_USAGE_BATTERY_SYSTEM_SMB_BATTERY_STATUS             = $02;
  HID_USAGE_BATTERY_SYSTEM_SMB_ALARM_WARNING              = $03;
  HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_MODE               = $04;
  HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_STATUS             = $05;
  HID_USAGE_BATTERY_SYSTEM_SMB_CHARGER_SPEC_INFO          = $06;
  HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_STATE             = $07;
  HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_PRESETS           = $08;
  HID_USAGE_BATTERY_SYSTEM_SMB_SELECTOR_INFO              = $09;
                                                          
  HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_1        = $10;
  HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_2        = $11;
  HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_3        = $12;
  HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_4        = $13;
  HID_USAGE_BATTERY_SYSTEM_OPTIONAL_MFG_FUNCTION_5        = $14;
  HID_USAGE_BATTERY_SYSTEM_CONNECTION_TO_SMBUS            = $15;
  HID_USAGE_BATTERY_SYSTEM_OUTPUT_CONNECTION              = $16;
  HID_USAGE_BATTERY_SYSTEM_CHARGER_CONNECTION             = $17;
  HID_USAGE_BATTERY_SYSTEM_BATTERY_INSERTION              = $18;
  HID_USAGE_BATTERY_SYSTEM_USE_NEXT                       = $19;
  HID_USAGE_BATTERY_SYSTEM_OK_TO_USE                      = $1A;
  HID_USAGE_BATTERY_SYSTEM_BATTERY_SUPPORTED              = $1B;
  HID_USAGE_BATTERY_SYSTEM_SELECTOR_REVISION              = $1C;
  HID_USAGE_BATTERY_SYSTEM_CHARGING_INDICATOR             = $1D;

  HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_ACCESS            = $28;
  HID_USAGE_BATTERY_SYSTEM_REMAINING_CAPACITY_LIMIT       = $29;
  HID_USAGE_BATTERY_SYSTEM_REMAINING_TIME_LIMIT           = $2A;
  HID_USAGE_BATTERY_SYSTEM_AT_RATE                        = $2B;
  HID_USAGE_BATTERY_SYSTEM_CAPACITY_MODE                  = $2C;
  HID_USAGE_BATTERY_SYSTEM_BROADCAST_TO_CHARGER           = $2D;
  HID_USAGE_BATTERY_SYSTEM_PRIMARY_BATTERY                = $2E;
  HID_USAGE_BATTERY_SYSTEM_CHARGE_CONTROLLER              = $2F;

  HID_USAGE_BATTERY_SYSTEM_TERMINATE_CHARGE               = $40;
  HID_USAGE_BATTERY_SYSTEM_TERMINATE_DISCHARGE            = $41;
  HID_USAGE_BATTERY_SYSTEM_BELOW_REMAINING_CAPACITY_LIMIT = $42;
  HID_USAGE_BATTERY_SYSTEM_REMAINING_TIME_LIMIT_EXPIRED   = $43;
  HID_USAGE_BATTERY_SYSTEM_CHARGING                       = $44;
  HID_USAGE_BATTERY_SYSTEM_DISCHARGING                    = $45;
  HID_USAGE_BATTERY_SYSTEM_FULLY_CHARGED                  = $46;
  HID_USAGE_BATTERY_SYSTEM_FULLY_DISCHARGED               = $47;
  HID_USAGE_BATTERY_SYSTEM_CONDITIONING_FLAG              = $48;
  HID_USAGE_BATTERY_SYSTEM_AT_RATE_OK                     = $49;
  HID_USAGE_BATTERY_SYSTEM_SMB_ERROR_CODE                 = $4A;
  HID_USAGE_BATTERY_SYSTEM_NEED_REPLACEMENT               = $4B;

  HID_USAGE_BATTERY_SYSTEM_AT_RATE_TIME_TO_FULL           = $60;
  HID_USAGE_BATTERY_SYSTEM_AT_RATE_TIME_TO_EMPTY          = $61;
  HID_USAGE_BATTERY_SYSTEM_AVERAGE_CURRENT                = $62;
  HID_USAGE_BATTERY_SYSTEM_MAX_ERROR                      = $63;
  HID_USAGE_BATTERY_SYSTEM_RELATIVE_STATE_OF_CHARGE       = $64;
  HID_USAGE_BATTERY_SYSTEM_ABSOLUTE_STATE_OF_CHARGE       = $65;
  HID_USAGE_BATTERY_SYSTEM_REMAINING_CAPACITY             = $66;
  HID_USAGE_BATTERY_SYSTEM_FULL_CHARGE_CAPACITY           = $67;
  HID_USAGE_BATTERY_SYSTEM_RUN_TIME_TO_EMPTY              = $68;
  HID_USAGE_BATTERY_SYSTEM_AVERAGE_TIME_TO_EMPTY          = $69;
  HID_USAGE_BATTERY_SYSTEM_AVERAGE_TIME_TO_FULL           = $6A;
  HID_USAGE_BATTERY_SYSTEM_CYCLE_COUNT                    = $6B;

  HID_USAGE_BATTERY_SYSTEM_BATT_PACK_MODEL_LEVEL          = $80;
  HID_USAGE_BATTERY_SYSTEM_INTERNAL_CHARGE_CONTROLLER     = $81;
  HID_USAGE_BATTERY_SYSTEM_PRIMARY_BATTERY_SUPPORT        = $82;
  HID_USAGE_BATTERY_SYSTEM_DESIGN_CAPACITY                = $83;
  HID_USAGE_BATTERY_SYSTEM_SPECIFICATION_INFO             = $84;
  HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_DATE              = $85;
  HID_USAGE_BATTERY_SYSTEM_SERIAL_NUMBER                  = $86;
  HID_USAGE_BATTERY_SYSTEM_I_MANUFACTURER_NAME            = $87;
  HID_USAGE_BATTERY_SYSTEM_I_DEVICE_NAME                  = $88;
  HID_USAGE_BATTERY_SYSTEM_I_DEVICE_CHEMISTERY            = $89;
  HID_USAGE_BATTERY_SYSTEM_MANUFACTURER_DATA              = $8A;
  HID_USAGE_BATTERY_SYSTEM_RECHARGABLE                    = $8B;
  HID_USAGE_BATTERY_SYSTEM_WARNING_CAPACITY_LIMIT         = $8c;
  HID_USAGE_BATTERY_SYSTEM_CAPACITY_GRANULARITY_1         = $8d;
  HID_USAGE_BATTERY_SYSTEM_CAPACITY_GRANULARITY_2         = $8E;
  HID_USAGE_BATTERY_SYSTEM_I_OEM_INFORMATION              = $8F;

  HID_USAGE_BATTERY_SYSTEM_INHIBIT_CHARGE                 = $C0;
  HID_USAGE_BATTERY_SYSTEM_ENABLE_POLLING                 = $C1;
  HID_USAGE_BATTERY_SYSTEM_RESET_TO_ZERO                  = $C2;

  HID_USAGE_BATTERY_SYSTEM_AC_PRESENT                     = $D0;
  HID_USAGE_BATTERY_SYSTEM_BATTERY_PRESENT                = $D1;
  HID_USAGE_BATTERY_SYSTEM_POWER_FAIL                     = $D2;
  HID_USAGE_BATTERY_SYSTEM_ALARM_INHIBITED                = $D3;
  HID_USAGE_BATTERY_SYSTEM_THERMISTOR_UNDER_RANGE         = $D4;
  HID_USAGE_BATTERY_SYSTEM_THERMISTOR_HOT                 = $D5;
  HID_USAGE_BATTERY_SYSTEM_THERMISTOR_COLD                = $D6;
  HID_USAGE_BATTERY_SYSTEM_THERMISTOR_OVER_RANGE          = $D7;
  HID_USAGE_BATTERY_SYSTEM_VOLTAGE_OUT_OF_RANGE           = $D8;
  HID_USAGE_BATTERY_SYSTEM_CURRENT_OUT_OF_RANGE           = $D9;
  HID_USAGE_BATTERY_SYSTEM_CURRENT_NOT_REGULATED          = $DA;
  HID_USAGE_BATTERY_SYSTEM_VOLTAGE_NOT_REGULATED          = $DB;
  HID_USAGE_BATTERY_SYSTEM_MASTER_MODE                    = $DC;

  HID_USAGE_BATTERY_SYSTEM_CHARGER_SELECTOR_SUPPORT       = $F0;
  HID_USAGE_BATTERY_SYSTEM_CHARGER_SPEC                   = $F1;
  HID_USAGE_BATTERY_SYSTEM_LEVEL_2                        = $F2;
  HID_USAGE_BATTERY_SYSTEM_LEVEL_3                        = $F3;

  // (rom) $F4 to $FF are reserved in "Usage Tables for HID Power Devices 1.0" (pdcv10.pdf)

  //
  // Barcode Scanner Page (0x8C)
  //
  HID_USAGE_BARCODE_SCANNER_UNDEFINED                                    = $000;
  HID_USAGE_BARCODE_SCANNER_BAR_CODE_BADGE_READER                        = $001;
  HID_USAGE_BARCODE_SCANNER_BAR_CODE_SCANNER                             = $002;
  HID_USAGE_BARCODE_SCANNER_DUMB_BAR_CODE_SCANNER                        = $003;
  HID_USAGE_BARCODE_SCANNER_CORDLESS_SCANNER_BASE                        = $004;
  HID_USAGE_BARCODE_SCANNER_BAR_CODE_SCANNER_CRADLE                      = $005;

  HID_USAGE_BARCODE_SCANNER_ATTRIBUTE_REPORT                             = $010;
  HID_USAGE_BARCODE_SCANNER_SETTINGS_REPORT                              = $011;
  HID_USAGE_BARCODE_SCANNER_SCANNED_DATA_REPORT                          = $012;
  HID_USAGE_BARCODE_SCANNER_RAW_SCANNED_DATA_REPORT                      = $013;
  HID_USAGE_BARCODE_SCANNER_TRIGGER_REPORT                               = $014;
  HID_USAGE_BARCODE_SCANNER_STATUS_REPORT                                = $015;
  HID_USAGE_BARCODE_SCANNER_UPC_EAN_CONTROL_REPORT                       = $016;
  HID_USAGE_BARCODE_SCANNER_EAN_2_3_LABEL_CONTROL_REPORT                 = $017;
  HID_USAGE_BARCODE_SCANNER_CODE_39_CONTROL_REPORT                       = $018;
  HID_USAGE_BARCODE_SCANNER_INTERLEAVED_2_OF_5_CONTROL_REPORT            = $019;
  HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5_CONTROL_REPORT               = $01A;
  HID_USAGE_BARCODE_SCANNER_MSI_PLESSEY_CONTROL_REPORT                   = $01B;
  HID_USAGE_BARCODE_SCANNER_CODABAR_CONTROL_REPORT                       = $01C;
  HID_USAGE_BARCODE_SCANNER_CODE_128_CONTROL_REPORT                      = $01D;
  HID_USAGE_BARCODE_SCANNER_MISC_1D_CONTROL_REPORT                       = $01E;
  HID_USAGE_BARCODE_SCANNER_2D_CONTROL_REPORT                            = $01F;

  HID_USAGE_BARCODE_SCANNER_AIMING_POINTER_MODE                          = $030;
  HID_USAGE_BARCODE_SCANNER_BAR_CODE_PRESENT_SENSOR                      = $031;
  HID_USAGE_BARCODE_SCANNER_CLASS_1A_LASER                               = $032;
  HID_USAGE_BARCODE_SCANNER_CLASS_2_LASER                                = $033;
  HID_USAGE_BARCODE_SCANNER_HEATER_PRESENT                               = $034;
  HID_USAGE_BARCODE_SCANNER_CONTACT_SCANNER                              = $035;
  HID_USAGE_BARCODE_SCANNER_ELECTRONIC_ARTICLE_SURVEILLANCE_NOTIFICATION = $036;
  HID_USAGE_BARCODE_SCANNER_CONSTANT_ARTICLE_SURVEILLANCE_NOTIFICATION   = $037;
  HID_USAGE_BARCODE_SCANNER_ERROR_INDICATION                             = $038;
  HID_USAGE_BARCODE_SCANNER_FIXED_BEEPER                                 = $039;
  HID_USAGE_BARCODE_SCANNER_GOOD_DECODE_INDICATION                       = $03A;
  HID_USAGE_BARCODE_SCANNER_HANDS_FREE_SCANNING                          = $03B;
  HID_USAGE_BARCODE_SCANNER_INTRINSICALLY_SAFE                           = $03C;
  HID_USAGE_BARCODE_SCANNER_KLASSE_EINS_LASER                            = $03D;
  HID_USAGE_BARCODE_SCANNER_LONG_RANGE_SCANNER                           = $03E;
  HID_USAGE_BARCODE_SCANNER_MIRROR_SPEED_CONTROL                         = $03F;
  HID_USAGE_BARCODE_SCANNER_NOT_ON_FILE_INDICATION                       = $040;
  HID_USAGE_BARCODE_SCANNER_PROGRAMMABLE_BEEPER                          = $041;
  HID_USAGE_BARCODE_SCANNER_TRIGGERLESS                                  = $042;
  HID_USAGE_BARCODE_SCANNER_WAND                                         = $043;
  HID_USAGE_BARCODE_SCANNER_WATER_RESISTANT                              = $044;
  HID_USAGE_BARCODE_SCANNER_MULTI_RANGE_SCANNER                          = $045;
  HID_USAGE_BARCODE_SCANNER_PROXIMITIY_SENSOR                            = $046;

  HID_USAGE_BARCODE_SCANNER_FRAGMENT_DECODING                            = $04D;
  HID_USAGE_BARCODE_SCANNER_SCANNER_READ_CONFIDENCE                      = $04E;
  HID_USAGE_BARCODE_SCANNER_DATA_PREFIX                                  = $04F;
  HID_USAGE_BARCODE_SCANNER_PREFIX_AIMI                                  = $050;
  HID_USAGE_BARCODE_SCANNER_PREFIX_NODE                                  = $051;
  HID_USAGE_BARCODE_SCANNER_PREFIX_PROPRIETARY                           = $052;

  HID_USAGE_BARCODE_SCANNER_ACTIVE_TIME                                  = $055;
  HID_USAGE_BARCODE_SCANNER_AIMING_LASER_PATTERN                         = $056;
  HID_USAGE_BARCODE_SCANNER_BAR_CODE_PRESENT                             = $057;
  HID_USAGE_BARCODE_SCANNER_BEEPER_STATE                                 = $058;
  HID_USAGE_BARCODE_SCANNER_LASER_ON_TIME                                = $059;
  HID_USAGE_BARCODE_SCANNER_LASER_STATE                                  = $05A;
  HID_USAGE_BARCODE_SCANNER_LOCKOUT_TIME                                 = $05B;
  HID_USAGE_BARCODE_SCANNER_MOTOR_STATE                                  = $05C;
  HID_USAGE_BARCODE_SCANNER_MOTOR_TIMEOUT                                = $05D;
  HID_USAGE_BARCODE_SCANNER_POWER_ON_RESET_SCANNER                       = $05E;
  HID_USAGE_BARCODE_SCANNER_PREVENT_READ_OF_BARCODES                     = $05F;
  HID_USAGE_BARCODE_SCANNER_INITIATE_BARCODE_READ                        = $060;
  HID_USAGE_BARCODE_SCANNER_TRIGGER_STATE                                = $061;
  HID_USAGE_BARCODE_SCANNER_TRIGGER_MODE                                 = $062;
  HID_USAGE_BARCODE_SCANNER_TM_BLINKING_LASER_ON                         = $063;
  HID_USAGE_BARCODE_SCANNER_TM_CONTINUOUS_LASER_ON                       = $064;
  HID_USAGE_BARCODE_SCANNER_TM_LASER_ON_WHILE_PULLED                     = $065;
  HID_USAGE_BARCODE_SCANNER_TM_LASER_STAYS_ON_AFTER_TRIGGER_RELEASE      = $066;

  HID_USAGE_BARCODE_SCANNER_COMMIT_PARAMETERS_TO_NVM                     = $06D;
  HID_USAGE_BARCODE_SCANNER_PARAMETER_SCANNING                           = $06E;
  HID_USAGE_BARCODE_SCANNER_PARAMETERS_CHANGED                           = $06F;
  HID_USAGE_BARCODE_SCANNER_SET_PARAMETER_DEFAULT_VALUES                 = $070;

  HID_USAGE_BARCODE_SCANNER_SCANNER_IN_CRADLE                            = $075;
  HID_USAGE_BARCODE_SCANNER_SCANNER_IN_RANGE                             = $076;

  HID_USAGE_BARCODE_SCANNER_AIM_DURATION                                 = $07A;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_LAMP_DURATION                      = $07B;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_LAMP_INTENSITY                     = $07C;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_LED                                = $07D;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_FREQUENCY                     = $07E;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_LENGTH                        = $07F;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_TONE_VOLUME                        = $080;

  HID_USAGE_BARCODE_SCANNER_NO_READ_MESSAGE                              = $082;
  HID_USAGE_BARCODE_SCANNER_NOT_ON_FILE_VOLUME                           = $083;
  HID_USAGE_BARCODE_SCANNER_POWERUP_BEEP                                 = $084;
  HID_USAGE_BARCODE_SCANNER_SOUND_ERROR_BEEP                             = $085;
  HID_USAGE_BARCODE_SCANNER_SOUND_GOOD_READ_BEEP                         = $086;
  HID_USAGE_BARCODE_SCANNER_SOUND_NOT_ON_FILE_BEEP                       = $087;
  HID_USAGE_BARCODE_SCANNER_GOOD_READ_WHEN_TO_WRITE                      = $088;
  HID_USAGE_BARCODE_SCANNER_GRWTI_AFTER_DECODE                           = $089;
  HID_USAGE_BARCODE_SCANNER_GRWTI_BEEP_LAMP_AFTER_TRANSMIT               = $08a;
  HID_USAGE_BARCODE_SCANNER_GRWTI_NO_BEEP_LAMP_USE_AT_ALL                = $08B;

  HID_USAGE_BARCODE_SCANNER_BOOKLAND_EAN                                 = $091;
  HID_USAGE_BARCODE_SCANNER_CONVERT_EAN_8_TO_13_TYPE                     = $092;
  HID_USAGE_BARCODE_SCANNER_CONVERT_UPC_A_TO_EAN_13                      = $093;
  HID_USAGE_BARCODE_SCANNER_CONVERT_UPC_E_TO_A                           = $094;
  HID_USAGE_BARCODE_SCANNER_EAN_13                                       = $095;
  HID_USAGE_BARCODE_SCANNER_EAN_8                                        = $096;
  HID_USAGE_BARCODE_SCANNER_EAN_99_128_MANDATORY                         = $097;
  HID_USAGE_BARCODE_SCANNER_EAN_99_P5_128_OPTIONAL                       = $098;

  HID_USAGE_BARCODE_SCANNER_UPC_EAN                                      = $09A;
  HID_USAGE_BARCODE_SCANNER_UPC_EAN_COUPON_CODE                          = $09B;
  HID_USAGE_BARCODE_SCANNER_UPC_EAN_PERIODICALS                          = $09C;
  HID_USAGE_BARCODE_SCANNER_UPC_A                                        = $09D;
  HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_128_MANDATORY                     = $09E;
  HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_128_OPTIONAL                      = $09F;
  HID_USAGE_BARCODE_SCANNER_UPC_A_WITH_P5_OPTIONAL                       = $0A0;
  HID_USAGE_BARCODE_SCANNER_UPC_E                                        = $0A1;
  HID_USAGE_BARCODE_SCANNER_UPC_E1                                       = $0A2;

  HID_USAGE_BARCODE_SCANNER_PERIODICAL                                   = $0A9;
  HID_USAGE_BARCODE_SCANNER_PERIODICAL_AUTODISCRIMINATE_2                = $0AA;
  HID_USAGE_BARCODE_SCANNER_PERIODICAL_ONLY_DECODE_WITH_2                = $0AB;
  HID_USAGE_BARCODE_SCANNER_PERIODICAL_IGNORE_2                          = $0AC;
  HID_USAGE_BARCODE_SCANNER_PERIODICAL_AUTODISCRIMINATE_5                = $0AD;
  HID_USAGE_BARCODE_SCANNER_PERIODICAL_ONLY_DECODE_WITH_5                = $0AE;
  HID_USAGE_BARCODE_SCANNER_PERIODICAL_IGNORE_5                          = $0AF;
  HID_USAGE_BARCODE_SCANNER_CHECK                                        = $0B0;
  HID_USAGE_BARCODE_SCANNER_CHECK_DISABLE_PRICE                          = $0B1;
  HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_4_DIGIT_PRICE                   = $0B2;
  HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_5_DIGIT_PRICE                   = $0B3;
  HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_EUROPEAN_4_DIGIT_PRICE          = $0B4;
  HID_USAGE_BARCODE_SCANNER_CHECK_ENABLE_EUROPEAN_5_DIGIT_PRICE          = $0B5;

  HID_USAGE_BARCODE_SCANNER_EAN_TWO_LABEL                                = $0B7;
  HID_USAGE_BARCODE_SCANNER_EAN_THREE_LABEL                              = $0B8;
  HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_1                           = $0B9;
  HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_2                           = $0BA;
  HID_USAGE_BARCODE_SCANNER_EAN_8_FLAG_DIGIT_3                           = $0BB;
  HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_1                          = $0BC;
  HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_2                          = $0BD;
  HID_USAGE_BARCODE_SCANNER_EAN_13_FLAG_DIGIT_3                          = $0BE;
  HID_USAGE_BARCODE_SCANNER_ADD_EAN_2_3_LABEL_DEFINITION                 = $0BF;
  HID_USAGE_BARCODE_SCANNER_CLEAR_ALL_EAN_2_3_LABEL_DEFINITIONS          = $0C0;

  HID_USAGE_BARCODE_SCANNER_CODABAR                                      = $0C3;
  HID_USAGE_BARCODE_SCANNER_CODE_128                                     = $0C4;

  HID_USAGE_BARCODE_SCANNER_CODE_39                                      = $0C7;
  HID_USAGE_BARCODE_SCANNER_CODE_93                                      = $0C8;
  HID_USAGE_BARCODE_SCANNER_FULL_ASCII_CONVERSION                        = $0C9;
  HID_USAGE_BARCODE_SCANNER_INTERLEAVED_2_OF_5                           = $0CA;
  HID_USAGE_BARCODE_SCANNER_ITALIAN_PHARMACY_CODE                        = $0CB;
  HID_USAGE_BARCODE_SCANNER_MSI_PLESSEY                                  = $0CC;
  HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5_IATA                         = $0CD;
  HID_USAGE_BARCODE_SCANNER_STANDARD_2_OF_5                              = $0CE;

  HID_USAGE_BARCODE_SCANNER_TRANSMIT_START_STOP                          = $0D3;
  HID_USAGE_BARCODE_SCANNER_TRI_OPTIC                                    = $0D4;
  HID_USAGE_BARCODE_SCANNER_UCC_EAN_128                                  = $0D5;
  HID_USAGE_BARCODE_SCANNER_CHECK_DIGIT                                  = $0D6;
  HID_USAGE_BARCODE_SCANNER_CD_DISABLE                                   = $0D7;
  HID_USAGE_BARCODE_SCANNER_CD_ENABLE_INTERLEAVED_2_OF_5_OPCC            = $0D8;
  HID_USAGE_BARCODE_SCANNER_CD_ENABLE_INTERLEAVED_2_OF_5_USS             = $0D9;
  HID_USAGE_BARCODE_SCANNER_CD_ENABLE_STANDARD_2_OF_5_OPCC               = $0DA;
  HID_USAGE_BARCODE_SCANNER_CD_ENABLE_STANDARD_2_OF_5_USS                = $0DB;
  HID_USAGE_BARCODE_SCANNER_CD_ENABLE_ONE_MSI_PLESSEY                    = $0DC;
  HID_USAGE_BARCODE_SCANNER_CD_ENABLE_TWO_MSI_PLESSEY                    = $0DD;
  HID_USAGE_BARCODE_SCANNER_CD_CODABAR_ENABLE                            = $0DE;
  HID_USAGE_BARCODE_SCANNER_CD_CODE_39_ENABLE                            = $0DF;

  HID_USAGE_BARCODE_SCANNER_TRANSMIT_CHECK_DIGIT                         = $0F0;
  HID_USAGE_BARCODE_SCANNER_DISABLE_CHECK_DIGIT_TRANSMIT                 = $0F1;
  HID_USAGE_BARCODE_SCANNER_ENABLE_CHECK_DIGIT_TRANSMIT                  = $0F2;

  HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_1                       = $0FB;
  HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_2                       = $0FC;
  HID_USAGE_BARCODE_SCANNER_SYMBOLOGY_IDENTIFIER_3                       = $0FD;
  HID_USAGE_BARCODE_SCANNER_DECODED_DATA                                 = $0FE;
  HID_USAGE_BARCODE_SCANNER_DECODED_DATA_CONTINUED                       = $0FF;
  HID_USAGE_BARCODE_SCANNER_BAR_SPACE_DATA                               = $100;
  HID_USAGE_BARCODE_SCANNER_SCANNER_DATA_ACCURACY                        = $101;
  HID_USAGE_BARCODE_SCANNER_RAW_DATA_POLARITY                            = $102;
  HID_USAGE_BARCODE_SCANNER_POLARITY_INVERTED_BAR_CODE                   = $103;
  HID_USAGE_BARCODE_SCANNER_POLARITY_NORMAL_BAR_CODE                     = $104;

  HID_USAGE_BARCODE_SCANNER_MINIMUM_LENGTH_TO_DECODE                     = $106;
  HID_USAGE_BARCODE_SCANNER_MAXIMUM_LENGTH_TO_DECODE                     = $107;
  HID_USAGE_BARCODE_SCANNER_FIRST_DISCRETE_LENGTH_TO_DECODE              = $108;
  HID_USAGE_BARCODE_SCANNER_SECOND_DISCRETE_LENGTH_TO_DECODE             = $109;
  HID_USAGE_BARCODE_SCANNER_DATA_LENGTH_METHOD                           = $10A;
  HID_USAGE_BARCODE_SCANNER_DLM_READ_ANY                                 = $10B;
  HID_USAGE_BARCODE_SCANNER_DLM_CHECK_IN_RANGE                           = $10C;
  HID_USAGE_BARCODE_SCANNER_DLM_CHECK_FOR_DISCRETE                       = $10D;
                                                                         
  HID_USAGE_BARCODE_SCANNER_AZTEC_CODE                                   = $110;
  HID_USAGE_BARCODE_SCANNER_BC412                                        = $111;
  HID_USAGE_BARCODE_SCANNER_CHANNEL_CODE                                 = $112;
  HID_USAGE_BARCODE_SCANNER_CODE_16                                      = $113;
  HID_USAGE_BARCODE_SCANNER_CODE_32                                      = $114;
  HID_USAGE_BARCODE_SCANNER_CODE_49                                      = $115;
  HID_USAGE_BARCODE_SCANNER_CODE_ONE                                     = $116;
  HID_USAGE_BARCODE_SCANNER_COLORCODE                                    = $117;
  HID_USAGE_BARCODE_SCANNER_DATA_MATRIX                                  = $118;
  HID_USAGE_BARCODE_SCANNER_MAXICODE                                     = $119;
  HID_USAGE_BARCODE_SCANNER_MICROPDF                                     = $11A;
  HID_USAGE_BARCODE_SCANNER_PDF_417                                      = $11B;
  HID_USAGE_BARCODE_SCANNER_POSICODE                                     = $11C;
  HID_USAGE_BARCODE_SCANNER_QR_CODE                                      = $11D;
  HID_USAGE_BARCODE_SCANNER_SUPERCODE                                    = $11E;
  HID_USAGE_BARCODE_SCANNER_ULTRACODE                                    = $11F;
  HID_USAGE_BARCODE_SCANNER_USD_5                                        = $120;
  HID_USAGE_BARCODE_SCANNER_VERICODE                                     = $121;

  // (rom) $122 to $FFFF are reserved in "HID Point of Sale Usage Tables 1.02" (pos1_02.pdf)

  //
  // Weighing Device Page (0x8D)
  //
  HID_USAGE_SCALE_UNDEFINED                    = $00;
  HID_USAGE_SCALE_WEIGHING_DEVICE              = $01;

  HID_USAGE_SCALE_SCALE_DEVICE_CLASS           = $20;
  HID_USAGE_SCALE_SCALE_CLASS_I_METRIC_CLASS   = $21;
  HID_USAGE_SCALE_SCALE_CLASS_I_METRIC         = $22;
  HID_USAGE_SCALE_SCALE_CLASS_II_METRIC        = $23;
  HID_USAGE_SCALE_SCALE_CLASS_III_METRIC       = $24;
  HID_USAGE_SCALE_SCALE_CLASS_IIIL_METRIC      = $25;
  HID_USAGE_SCALE_SCALE_CLASS_IV_METRIC        = $26;
  HID_USAGE_SCALE_SCALE_CLASS_III_ENGLISH      = $27;
  HID_USAGE_SCALE_SCALE_CLASS_IIIL_ENGLISH     = $28;
  HID_USAGE_SCALE_SCALE_CLASS_IV_ENGLISH       = $29;
  HID_USAGE_SCALE_SCALE_CLASS_GENERIC          = $2A;

  HID_USAGE_SCALE_SCALE_ATTRIBUTE_REPORT       = $30;
  HID_USAGE_SCALE_SCALE_CONTROL_REPORT         = $31;
  HID_USAGE_SCALE_SCALE_DATA_REPORT            = $32;
  HID_USAGE_SCALE_SCALE_STATUS_REPORT          = $33;
  HID_USAGE_SCALE_SCALE_WEIGHT_LIMIT_REPORT    = $34;
  HID_USAGE_SCALE_SCALE_STATISTICS_REPORT      = $35;

  HID_USAGE_SCALE_DATA_WEIGHT                  = $40;
  HID_USAGE_SCALE_DATA_SCALING                 = $41;

  HID_USAGE_SCALE_WEIGHT_UNIT_CLASS            = $50;
  HID_USAGE_SCALE_WEIGHT_UNIT_MILLIGRAM        = $51;
  HID_USAGE_SCALE_WEIGHT_UNIT_GRAM             = $52;
  HID_USAGE_SCALE_WEIGHT_UNIT_KILOGRAM         = $53;
  HID_USAGE_SCALE_WEIGHT_UNIT_CARATS           = $54;
  HID_USAGE_SCALE_WEIGHT_UNIT_TAELS            = $55;
  HID_USAGE_SCALE_WEIGHT_UNIT_GRAINS           = $56;
  HID_USAGE_SCALE_WEIGHT_UNIT_PENNYWEIGHTS     = $57;
  HID_USAGE_SCALE_WEIGHT_UNIT_METRIC_TON       = $58;
  HID_USAGE_SCALE_WEIGHT_UNIT_AVOIR_TON        = $59;
  HID_USAGE_SCALE_WEIGHT_UNIT_TROY_OUNCE       = $5A;
  HID_USAGE_SCALE_WEIGHT_UNIT_OUNCE            = $5B;
  HID_USAGE_SCALE_WEIGHT_UNIT_POUND            = $5C;

  HID_USAGE_SCALE_CALIBRATION_COUNT            = $60;
  HID_USAGE_SCALE_RE_ZERO_COUNT                = $61;

  HID_USAGE_SCALE_SCALE_STATUS_CLASS           = $70;
  HID_USAGE_SCALE_SCS_FAULT                    = $71;
  HID_USAGE_SCALE_SCS_STABLE_AT_CENTER_OF_ZERO = $72;
  HID_USAGE_SCALE_SCS_IN_MOTION                = $73;
  HID_USAGE_SCALE_SCS_WEIGHT_STABLE            = $74;
  HID_USAGE_SCALE_SCS_UNDER_ZERO               = $75;
  HID_USAGE_SCALE_SCS_OVER_WEIGHT_LIMIT        = $76;
  HID_USAGE_SCALE_SCS_REQUIRES_CALIBRATION     = $77;
  HID_USAGE_SCALE_SCS_REQUIRES_REZEROING       = $78;

  HID_USAGE_SCALE_ZERO_SCALE                   = $80;
  HID_USAGE_SCALE_ENFORCED_ZERO_RETURN         = $81;

  // (rom) $82 to $FFFF are reserved in "HID Point of Sale Usage Tables 1.02" (pos1_02.pdf)

  //
  // Magnetic Stripe Reader Page (0x8E)
  //
  HID_USAGE_MSR_UNDEFINED            = $00;
  HID_USAGE_MSR_MSR_DEVICE_READ_ONLY = $01;

  HID_USAGE_MSR_TRACK_1_LENGTH       = $11;
  HID_USAGE_MSR_TRACK_2_LENGTH       = $12;
  HID_USAGE_MSR_TRACK_3_LENGTH       = $13;
  HID_USAGE_MSR_TRACK_JIS_LENGTH     = $14;

  HID_USAGE_MSR_TRACK_DATA           = $20;
  HID_USAGE_MSR_TRACK_1_DATA         = $21;
  HID_USAGE_MSR_TRACK_2_DATA         = $22;
  HID_USAGE_MSR_TRACK_3_DATA         = $23;
  HID_USAGE_MSR_TRACK_JIS_DATA       = $24;

  // (rom) $25 to $FFFF are reserved in ""HID Point of Sale Usage Tables 1.02" (pos1_02.pdf)

implementation

end.
