{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMessageBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvMessageBox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDlg, JvTypes, JvComponent;

type
  TJvMessageBox = class(TJvComponent)
  private
    FText: string;
    FTitle: string;
    FDisplay: TButtonDisplay;
    FOptions: TButtonOptions;
    FStyle: TButtonStyle;
    FDefault: TDefault;
    FModality: TModality;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Text: string read FText write FText;
    property Title: string read FTitle write FTitle;
    property Style: TButtonStyle read FStyle write FStyle default bsOkCancel;
    property Display: TButtonDisplay read FDisplay write FDisplay default bdIconQuestion;
    property DefaultButton: TDefault read FDefault write FDefault default dbButton1;
    property Modality: TModality read FModality write FModality default bmApplModal;
    property Options: TButtonOptions read FOptions write FOptions;
    function Execute: TButtonResult;
  end;

implementation

{**************************************************}

constructor TJvMessageBox.Create(AOwner: TComponent);
begin
  inherited;
  FStyle := bsOkCancel;
  FDisplay := bdIconQuestion;
  FDefault := dbButton1;
  FModality := bmApplModal;
end;

{**************************************************}

function TJvMessageBox.Execute: TButtonResult;
var
  Flags: Integer;
begin
  Flags := 0;
  case FStyle of
    bsAbortRetryIgnore:
      Flags := MB_ABORTRETRYIGNORE;
    bsOk:
      Flags := MB_OK;
    bsOkCancel:
      Flags := MB_OKCANCEL;
    bsRetryCancel:
      Flags := MB_RETRYCANCEL;
    bsYesNo:
      Flags := MB_YESNO;
    bsYesNoCancel:
      Flags := MB_YESNOCANCEL;
  end;
  case FDisplay of
    bdIconExclamation:
      Flags := Flags + MB_ICONEXCLAMATION;
    bdIconWarning:
      Flags := Flags + MB_ICONWARNING;
    bdIconInformation:
      Flags := Flags + MB_ICONINFORMATION;
    bdIconAsterisk:
      Flags := Flags + MB_ICONASTERISK;
    bdIconQuestion:
      Flags := Flags + MB_ICONQUESTION;
    bdIconStop:
      Flags := Flags + MB_ICONSTOP;
    bdIconError:
      Flags := Flags + MB_ICONERROR;
    bdIconHand:
      Flags := Flags + MB_ICONHAND;
  end;
  case FDefault of
    dbButton1:
      Flags := Flags + MB_DEFBUTTON1;
    dbButton2:
      Flags := Flags + MB_DEFBUTTON2;
    dbButton3:
      Flags := Flags + MB_DEFBUTTON3;
    dbButton4:
      Flags := Flags + MB_DEFBUTTON4;
  end;
  case FModality of
    bmApplModal:
      Flags := Flags + MB_APPLMODAL;
    bmSystemModal:
      Flags := Flags + MB_SYSTEMMODAL;
    bmTaskModal:
      Flags := Flags + MB_TASKMODAL;
  end;
  if boDefaultDesktopOnly in FOptions then
    Flags := Flags + MB_DEFAULT_DESKTOP_ONLY;
  if boHelp in FOptions then
    Flags := Flags + MB_HELP;
  if boRight in FOptions then
    Flags := Flags + MB_RIGHT;
  if boRtlReading in FOptions then
    Flags := Flags + MB_RTLREADING;
  if boSetForeground in FOptions then
    Flags := Flags + MB_SETFOREGROUND;
  if boTopMost in FOptions then
    Flags := Flags + MB_TOPMOST;

  case MessageBox(TForm(Self.Owner).Handle, PChar(FText), PChar(FTitle), Flags) of
    IDABORT:
      Result := brAbort;
    IDCANCEL:
      Result := brCancel;
    IDIGNORE:
      Result := brIgnore;
    IDNO:
      Result := brNo;
    IDOK:
      Result := brOk;
    IDRETRY:
      Result := brRetry;
    IDYES:
      Result := brYes;
  else
    Result := brAbort;
  end;
end;

end.
