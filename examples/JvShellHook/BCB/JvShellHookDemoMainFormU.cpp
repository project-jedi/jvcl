/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

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

******************************************************************/
// $Id$
//---------------------------------------------------------------------------
/*
   Note : if we want to use WinXP definitions there is need to
          set _WIN32_WINNT define to 0x501.
          Default value is not 0x501
*/
#if (_WIN32_WINNT<=0x500)
#define  _WIN32_WINNT  (0x501)
#endif

#include <vcl.h>
#include <windows.h>
#pragma hdrstop

// (outchy) patch for compiling with BCB5 and BCB6
/*******************************************************************************
  Note : You may update "winuser.*" files in $(BCB)\include
          directory from older BCB distribution of Win32API
  by newer MS SDK (http://www.microsoft.com/msdownload/platformsdk/sdkupdate/).
*******************************************************************************/

#define SDKUPDATE "read the note above about SDK update"

#ifndef HSHELL_SYSMENU
#pragma message (SDKUPDATE)
#define HSHELL_SYSMENU 9
#endif

#ifndef HSHELL_ENDTASK
#pragma message (SDKUPDATE)
#define HSHELL_ENDTASK 10
#endif

#ifndef HSHELL_WINDOWREPLACED
#pragma message (SDKUPDATE)
#define HSHELL_WINDOWREPLACED 13
#endif

#ifndef HSHELL_WINDOWREPLACING
#pragma message (SDKUPDATE)
#define HSHELL_WINDOWREPLACING 14
#endif

#ifndef HSHELL_HIGHBIT
#pragma message (SDKUPDATE)
#define HSHELL_HIGHBIT            0x8000
#endif

#ifndef HSHELL_FLASH
#pragma message (SDKUPDATE)
#define HSHELL_FLASH              (HSHELL_REDRAW|HSHELL_HIGHBIT)
#endif

#ifndef HSHELL_RUDEAPPACTIVATED
#pragma message (SDKUPDATE)
#define HSHELL_RUDEAPPACTIVATED   (HSHELL_WINDOWACTIVATED|HSHELL_HIGHBIT)
#endif

#ifndef APPCOMMAND_MICROPHONE_VOLUME_MUTE
#pragma message (SDKUPDATE)
#define APPCOMMAND_MICROPHONE_VOLUME_MUTE 24
#endif

#ifndef APPCOMMAND_MICROPHONE_VOLUME_DOWN
#pragma message (SDKUPDATE)
#define APPCOMMAND_MICROPHONE_VOLUME_DOWN 25
#endif

#ifndef APPCOMMAND_MICROPHONE_VOLUME_UP
#pragma message (SDKUPDATE)
#define APPCOMMAND_MICROPHONE_VOLUME_UP   26
#endif

#ifndef APPCOMMAND_HELP
#pragma message (SDKUPDATE)
#define APPCOMMAND_HELP                   27
#endif

#ifndef APPCOMMAND_FIND
#pragma message (SDKUPDATE)
#define APPCOMMAND_FIND                   28
#endif

#ifndef APPCOMMAND_NEW
#pragma message (SDKUPDATE)
#define APPCOMMAND_NEW                    29
#endif

#ifndef APPCOMMAND_OPEN
#pragma message (SDKUPDATE)
#define APPCOMMAND_OPEN                   30
#endif

#ifndef APPCOMMAND_CLOSE
#pragma message (SDKUPDATE)
#define APPCOMMAND_CLOSE                  31
#endif

#ifndef APPCOMMAND_SAVE
#pragma message (SDKUPDATE)
#define APPCOMMAND_SAVE                   32
#endif

#ifndef APPCOMMAND_PRINT
#pragma message (SDKUPDATE)
#define APPCOMMAND_PRINT                  33
#endif

#ifndef APPCOMMAND_UNDO
#pragma message (SDKUPDATE)
#define APPCOMMAND_UNDO                   34
#endif

#ifndef APPCOMMAND_REDO
#pragma message (SDKUPDATE)
#define APPCOMMAND_REDO                   35
#endif

#ifndef APPCOMMAND_COPY
#pragma message (SDKUPDATE)
#define APPCOMMAND_COPY                   36
#endif

#ifndef APPCOMMAND_CUT
#pragma message (SDKUPDATE)
#define APPCOMMAND_CUT                    37
#endif

#ifndef APPCOMMAND_PASTE
#pragma message (SDKUPDATE)
#define APPCOMMAND_PASTE                  38
#endif

#ifndef APPCOMMAND_REPLY_TO_MAIL
#pragma message (SDKUPDATE)
#define APPCOMMAND_REPLY_TO_MAIL          39
#endif

#ifndef APPCOMMAND_FORWARD_MAIL
#pragma message (SDKUPDATE)
#define APPCOMMAND_FORWARD_MAIL           40
#endif

#ifndef APPCOMMAND_SEND_MAIL
#pragma message (SDKUPDATE)
#define APPCOMMAND_SEND_MAIL              41
#endif

#ifndef APPCOMMAND_SPELL_CHECK
#pragma message (SDKUPDATE)
#define APPCOMMAND_SPELL_CHECK            42
#endif

#ifndef APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE
#pragma message (SDKUPDATE)
#define APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE    43
#endif

#ifndef APPCOMMAND_MIC_ON_OFF_TOGGLE
#pragma message (SDKUPDATE)
#define APPCOMMAND_MIC_ON_OFF_TOGGLE      44
#endif

#ifndef APPCOMMAND_CORRECTION_LIST
#pragma message (SDKUPDATE)
#define APPCOMMAND_CORRECTION_LIST        45
#endif

#ifndef APPCOMMAND_MEDIA_PLAY
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_PLAY             46
#endif

#ifndef APPCOMMAND_MEDIA_PAUSE
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_PAUSE            47
#endif

#ifndef APPCOMMAND_MEDIA_RECORD
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_RECORD           48
#endif

#ifndef APPCOMMAND_MEDIA_FAST_FORWARD
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_FAST_FORWARD     49
#endif

#ifndef APPCOMMAND_MEDIA_REWIND
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_REWIND           50
#endif

#ifndef APPCOMMAND_MEDIA_CHANNEL_UP
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_CHANNEL_UP       51
#endif

#ifndef APPCOMMAND_MEDIA_CHANNEL_DOWN
#pragma message (SDKUPDATE)
#define APPCOMMAND_MEDIA_CHANNEL_DOWN     52
#endif


#include "JvShellHookDemoMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvShellHook"
#pragma resource "*.dfm"
TJvShellHookDemoMainForm *JvShellHookDemoMainForm;

//---------------------------------------------------------------------------
__fastcall TJvShellHookDemoMainForm::TJvShellHookDemoMainForm(TComponent* Owner)
        : TForm(Owner)
{

    SH = new TJvShellHook(this);
    SH->OnShellMessage = DoShellMessage;
    chkActive->Enabled = InitJvShellHooks;
#ifdef COMPILER6_UP /* see headre file */
    lvMessages->BorderStyle = bsNone;
    //lvMessages->BevelKind   = bkFlat;
    //lvMessages->BevelInner  = bvNone;
#else
    lvMessages->BorderStyle = bsSingle;
#endif
}
//---------------------------------------------------------------------------


__fastcall TJvShellHookDemoMainForm::~TJvShellHookDemoMainForm(void)
{
  delete SH;        
}


void __fastcall TJvShellHookDemoMainForm::DoShellMessage(TObject * Sender, TMessage &Message)
{
  AnsiString S;
//begin
//  with Message do
//  begin
    switch(Message.WParam)
    {
//    case wParam of
      case HSHELL_WINDOWCREATED      :{ S = "HSHELL_WINDOWCREATED";}break;
      case HSHELL_WINDOWDESTROYED    :{ S = "HSHELL_WINDOWDESTROYED";}break;
      case HSHELL_ACTIVATESHELLWINDOW:{ S = "HSHELL_ACTIVATESHELLWINDOW";}break;
      case HSHELL_WINDOWACTIVATED    :
       {
         S = "HSHELL_WINDOWACTIVATED";
         if( reinterpret_cast<HWND >(Message.LParam) == Application->Handle )
         {
           S = S + " (this)";
         }
       }break;
      case HSHELL_GETMINRECT         :{ S = "HSHELL_GETMINRECT";}break;
      case HSHELL_REDRAW:
       {
         if( chkNoRedraw->Checked )
         {
           return; //Exit
         }
         else
         {
           S = "HSHELL_REDRAW";
         }
       }break;
      case HSHELL_TASKMAN            :{ S = "HSHELL_TASKMAN";}break;
      case HSHELL_LANGUAGE           :{ S = "HSHELL_LANGUAGE";}break;
      case HSHELL_SYSMENU            :{ S = "HSHELL_SYSMENU";}break;
      case HSHELL_ENDTASK            :{ S = "HSHELL_ENDTASK";}break;
      case HSHELL_ACCESSIBILITYSTATE :{ S = "HSHELL_ACCESSIBILITYSTATE";}break;
      case HSHELL_WINDOWREPLACED     :{ S = "HSHELL_WINDOWREPLACED";}break;
      case HSHELL_WINDOWREPLACING    :{ S = "HSHELL_WINDOWREPLACING";}break;
      case HSHELL_FLASH              :{ S = "HSHELL_FLASH";}break;
      case HSHELL_RUDEAPPACTIVATED   :{ S = "HSHELL_RUDEAPPACTIVATED";}break;
      case HSHELL_APPCOMMAND         :{ S = GetAppCommand(Message.LParam);}break;
      default:
       {
        S = Format("Unknown command ($%.8x)", ARRAYOFCONST( (Message.WParam) ) );
       }break;
    }
    lvMessages->Items->Add()->Caption = S;
  int siRowNum = lvMessages->Items->Count - 1;
    lvMessages->Items->Item[siRowNum]->SubItems->Add(IntToStr(Message.WParam));
    lvMessages->Items->Item[siRowNum]->SubItems->Add(IntToStr(Message.LParam));
    lvMessages->Items->Item[siRowNum]->SubItems->Add(IntToStr(Message.Result));
    lvMessages->Items->Item[siRowNum]->MakeVisible(false);
    lvMessages->Items->Item[siRowNum]->Selected = true;
    lvMessages->Items->Item[siRowNum]->Focused  = true;

}


AnsiString TJvShellHookDemoMainForm::GetAppCommand(int LParam)
{
 AnsiString tmp;
//begin
//  case GET_APPCOMMAND_LPARAM(lParam) of
   switch(GET_APPCOMMAND_LPARAM(LParam))
   {
     case APPCOMMAND_BROWSER_BACKWARD   :{ tmp = "APPCOMMAND_BROWSER_BACKWARD";}break;
     case APPCOMMAND_BROWSER_FORWARD    :{ tmp = "APPCOMMAND_BROWSER_FORWARD"; }break;
     case APPCOMMAND_BROWSER_REFRESH    :{ tmp = "APPCOMMAND_BROWSER_REFRESH"; }break;
     case APPCOMMAND_BROWSER_STOP       :{ tmp = "APPCOMMAND_BROWSER_STOP";    }break;
     case APPCOMMAND_BROWSER_SEARCH     :{ tmp = "APPCOMMAND_BROWSER_SEARCH";  }break;
     case APPCOMMAND_BROWSER_FAVORITES  :
                                        { tmp = "APPCOMMAND_BROWSER_FAVORITES";}break;
     case APPCOMMAND_BROWSER_HOME       :{ tmp = "APPCOMMAND_BROWSER_HOME";    }break;
     case APPCOMMAND_VOLUME_MUTE        :{ tmp = "APPCOMMAND_VOLUME_MUTE";     }break;
     case APPCOMMAND_VOLUME_DOWN        :{ tmp = "APPCOMMAND_VOLUME_DOWN";     }break;
     case APPCOMMAND_VOLUME_UP          :{ tmp = "APPCOMMAND_VOLUME_UP";       }break;
     case APPCOMMAND_MEDIA_NEXTTRACK    :{ tmp = "APPCOMMAND_MEDIA_NEXTTRACK"; }break;
     case APPCOMMAND_MEDIA_PREVIOUSTRACK:
                                  { tmp =     "APPCOMMAND_MEDIA_PREVIOUSTRACK";}break;
     case APPCOMMAND_MEDIA_STOP         :{ tmp = "APPCOMMAND_MEDIA_STOP";      }break;
     case APPCOMMAND_MEDIA_PLAY_PAUSE   :{ tmp = "APPCOMMAND_MEDIA_PLAY_PAUSE";}break;
     case APPCOMMAND_LAUNCH_MAIL        :{ tmp = "APPCOMMAND_LAUNCH_MAIL";     }break;
     case APPCOMMAND_LAUNCH_MEDIA_SELECT:
                                  { tmp = "APPCOMMAND_LAUNCH_MEDIA_SELECT";    }break;
     case APPCOMMAND_LAUNCH_APP1        :{ tmp = "APPCOMMAND_LAUNCH_APP1";     }break;
     case APPCOMMAND_LAUNCH_APP2        :{ tmp = "APPCOMMAND_LAUNCH_APP2";     }break;
     case APPCOMMAND_BASS_DOWN          :{ tmp = "APPCOMMAND_BASS_DOWN";       }break;
     case APPCOMMAND_BASS_BOOST         :{ tmp = "APPCOMMAND_BASS_BOOST";      }break;
     case APPCOMMAND_BASS_UP            :{ tmp = "APPCOMMAND_BASS_UP";         }break;
     case APPCOMMAND_TREBLE_DOWN        :{ tmp = "APPCOMMAND_TREBLE_DOWN";     }break;
     case APPCOMMAND_TREBLE_UP          :{ tmp = "APPCOMMAND_TREBLE_UP";       }break;
     case APPCOMMAND_MICROPHONE_VOLUME_MUTE:
                                  { tmp = "APPCOMMAND_MICROPHONE_VOLUME_MUTE"; }break;
     case APPCOMMAND_MICROPHONE_VOLUME_DOWN:
                                  { tmp = "APPCOMMAND_MICROPHONE_VOLUME_DOWN"; }break;
     case APPCOMMAND_MICROPHONE_VOLUME_UP:
                                  { tmp =   "APPCOMMAND_MICROPHONE_VOLUME_UP"; }break;
     case APPCOMMAND_HELP               :{ tmp = "APPCOMMAND_HELP";            }break;
     case APPCOMMAND_FIND               :{ tmp = "APPCOMMAND_FIND";            }break;
     case APPCOMMAND_NEW                :{ tmp = "APPCOMMAND_NEW";             }break;
     case APPCOMMAND_OPEN               :{ tmp = "APPCOMMAND_OPEN";            }break;
     case APPCOMMAND_CLOSE              :{ tmp = "APPCOMMAND_CLOSE";           }break;
     case APPCOMMAND_SAVE               :{ tmp = "APPCOMMAND_SAVE";            }break;
     case APPCOMMAND_PRINT              :{ tmp = "APPCOMMAND_PRINT";           }break;
     case APPCOMMAND_UNDO               :{ tmp = "APPCOMMAND_UNDO";            }break;
     case APPCOMMAND_REDO               :{ tmp = "APPCOMMAND_REDO";            }break;
     case APPCOMMAND_COPY               :{ tmp = "APPCOMMAND_COPY";            }break;
     case APPCOMMAND_CUT                :{ tmp = "APPCOMMAND_CUT";             }break;
     case APPCOMMAND_PASTE              :{ tmp = "APPCOMMAND_PASTE";           }break;
     case APPCOMMAND_REPLY_TO_MAIL      :{ tmp = "APPCOMMAND_REPLY_TO_MAIL";   }break;
     case APPCOMMAND_FORWARD_MAIL       :{ tmp = "APPCOMMAND_FORWARD_MAIL";    }break;
     case APPCOMMAND_SEND_MAIL          :{ tmp = "APPCOMMAND_SEND_MAIL";       }break;
     case APPCOMMAND_SPELL_CHECK        :{ tmp = "APPCOMMAND_SPELL_CHECK";     }break;
     case APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE:
                       { tmp = "APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE"; }break;
     case APPCOMMAND_MIC_ON_OFF_TOGGLE  :
                                  { tmp =       "APPCOMMAND_MIC_ON_OFF_TOGGLE";}break;
     case APPCOMMAND_CORRECTION_LIST    :{ tmp = "APPCOMMAND_CORRECTION_LIST"; }break;
     case APPCOMMAND_MEDIA_PLAY         :{ tmp = "APPCOMMAND_MEDIA_PLAY";      }break;
     case APPCOMMAND_MEDIA_PAUSE        :{ tmp = "APPCOMMAND_MEDIA_PAUSE";     }break;
     case APPCOMMAND_MEDIA_RECORD       :{ tmp = "APPCOMMAND_MEDIA_RECORD";    }break;
     case APPCOMMAND_MEDIA_FAST_FORWARD :
                                  { tmp =      "APPCOMMAND_MEDIA_FAST_FORWARD";}break;
     case APPCOMMAND_MEDIA_REWIND       :{ tmp = "APPCOMMAND_MEDIA_REWIND";    }break;
     case APPCOMMAND_MEDIA_CHANNEL_UP   :{ tmp = "APPCOMMAND_MEDIA_CHANNEL_UP";}break;
     case APPCOMMAND_MEDIA_CHANNEL_DOWN :
                                  { tmp =      "APPCOMMAND_MEDIA_CHANNEL_DOWN";}break;
    default:
    {
     tmp = Format("Unknown ($%.8x)", ARRAYOFCONST( (LParam) ) );
    }break;
   }

  return ( Format("HSHELL_APPCOMMAND: %s", ARRAYOFCONST((tmp)) ) );
}
void __fastcall TJvShellHookDemoMainForm::chkActiveClick(TObject *Sender)
{
  SH->Active = chkActive->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TJvShellHookDemoMainForm::btnClearClick(TObject *Sender)
{
  lvMessages->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TJvShellHookDemoMainForm::lvMessagesResize(TObject *Sender)
{
  lvMessages->Columns->Items[3]->Width = -2;
}
//---------------------------------------------------------------------------
