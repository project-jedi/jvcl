{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit CaptionEditForm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls;

type
  TCaptionEditDlg = class(TForm)
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    CaptionEdit: TMemo;
    FontBtn: TButton;
    FontDialog1: TFontDialog;
    procedure FontBtnClick(Sender: TObject);
  private
  public
    class procedure NewCaption(var TheCaption : string;TheFont : TFont);
  end;


implementation

{$R *.xfm}

class procedure TCaptionEditDlg.NewCaption(var TheCaption : string;TheFont : TFont);
begin {NewCaption}
  with TCaptionEditDlg.Create(Application) do begin
    try
      CaptionEdit.Text := TheCaption;
      FontDialog1.Font := TheFont;

      if ShowModal = mrOk then begin
        TheCaption := CaptionEdit.Text;
        TheFont.Assign(CaptionEdit.Font);
      end;
    finally
      Release;
    end;
  end;
end;  {NewCaption}


procedure TCaptionEditDlg.FontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    CaptionEdit.Font := FontDialog1.Font;
  end;
end;


end.
