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

unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, QActnList, QImgList, JvQXPCore, JvQXPCheckCtrls, JvQXPButtons, QExtCtrls,
  QStdCtrls, JvQXPContainer, JvQComponent, JvQExControls;

type
{ TfrmMain }

  TfrmMain = class(TForm)
    acBtn1: TAction;
    acBtn2: TAction;
    acBtn3: TAction;
    acBtn4: TAction;
    aclMain: TActionList;
    btn1: TJvXPButton;
    btn2: TJvXPButton;
    btn3: TJvXPButton;
    btn4: TJvXPButton;
    btnCancel: TJvXPButton;
    btnClose: TJvXPToolButton;
    btnLeft: TJvXPToolButton;
    btnOK: TJvXPButton;
    btnRight: TJvXPToolButton;
    chk1: TJvXPCheckbox;
    chk2: TJvXPCheckbox;
    chkOfficeStyle: TJvXPCheckbox;
    chkToogleEnable: TJvXPCheckbox;
    cntHeader: TJvXPContainer;
    cntNetHeader: TJvXPContainer;
    cntNetPanel: TJvXPContainer;
    dxToolButton1: TJvXPToolButton;
    dxToolButton2: TJvXPToolButton;
    dxToolButton3: TJvXPToolButton;
    dxToolButton4: TJvXPToolButton;
    dxToolButton5: TJvXPToolButton;
    imgConfigure: TImage;
    imlMain: TImageList;
    lbBrowse: TLabel;
    lbConfigure: TLabel;
    lbInternalPage: TLabel;
    lbWebEditor: TLabel;
    shpSeperator: TShape;
    styleOffice: TJvXPStyleManager;
    JvXPButton1: TJvXPButton;
    procedure FormCreate(Sender: TObject);
    procedure acBtn1Execute(Sender: TObject);
    procedure acBtn3Execute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure chkOfficeStyleClick(Sender: TObject);
    procedure chkToogleEnableClick(Sender: TObject);
    procedure cntHeaderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cntHeaderPaint(Sender: TObject; Rect: TRect; ACanvas: TCanvas; AFont: TFont);
    procedure cntNetPanelPaint(Sender: TObject; Rect: TRect; ACanvas: TCanvas; AFont: TFont);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.xfm}

{ TfrmMain }

{-----------------------------------------------------------------------------
  Procedure: RemoveTitleBar
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: hWindow: THANDLE; Hide: boolean = True
  Result:    DWORD
-----------------------------------------------------------------------------}
(*
function RemoveTitleBar(hWindow: THANDLE; Hide: boolean = True): DWORD;
var
  R: TRect;
begin
  Result := GetWindowLong(hWindow, GWL_STYLE);
  if (Hide) then
    Result := Result and not WS_CAPTION
  else
    Result := Result or WS_CAPTION;
  GetClientRect(hWindow, R);
  SetWindowLong(hWindow, GWL_STYLE, Result);
  AdjustWindowRect(R, Result, boolean(GetMenu(hWindow)));
  SetWindowPos(hWindow, 0, 0, 0, (R.Right - R.Left), (R.Bottom - R.Top),
    SWP_NOMOVE or SWP_NOZORDER or SWP_FRAMECHANGED or SWP_NOSENDCHANGING);
end;
*)
{-----------------------------------------------------------------------------
  Procedure: TfrmMain.FormCreate
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
//  RemoveTitleBar(Handle);
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.cntHeaderPaint
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject; Rect: TRect; ACanvas: TCanvas; AFont: TFont
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.cntHeaderPaint(Sender: TObject; Rect: TRect;
  ACanvas: TCanvas; AFont: TFont);
var
  i: Integer;
begin
  with ACanvas do
  begin
    for i := Rect.Top to Rect.Bottom do
    begin
      Pen.Color := clGray;
      Rectangle(Rect.Left + 1, Rect.Top + i shl 1, Rect.Right - 1,
        Rect.Top + i shl 1 + 1);
    end;
    Brush.Color := clBtnFace;
    DrawText(ACanvas, ' ' + Application.Title + ' ', -1, Rect, DT_SINGLELINE or
      DT_VCENTER or DT_CENTER or DT_END_ELLIPSIS);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.cntHeaderMouseDown
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.cntHeaderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
 // Perform(WM_SYSCOMMAND, $F012, 0);
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.btnCloseClick
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.cntNetPanelPaint
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject; Rect: TRect; ACanvas: TCanvas; AFont: TFont
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.cntNetPanelPaint(Sender: TObject; Rect: TRect;
  ACanvas: TCanvas; AFont: TFont);
var
  Control: TControl;
  EdgeColor: TColor;
begin
  Control := TControl(Sender);
  EdgeColor := TForm(Control.Parent).Color;
  ACanvas.Pixels[0, 0] := EdgeColor;
  ACanvas.Pixels[Control.Width - 1, 0] := EdgeColor;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.acGenerateExecute
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.acBtn1Execute(Sender: TObject);
begin
  //
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.acBtn3Execute
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.acBtn3Execute(Sender: TObject);
begin
  //
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.chkToogleEnableClick
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.chkToogleEnableClick(Sender: TObject);
begin
  acBtn1.Enabled := not chkToogleEnable.Checked;
  acBtn3.Enabled := not chkToogleEnable.Checked;
end;

{-----------------------------------------------------------------------------
  Procedure: TfrmMain.chkOfficeStyleClick
  Author:    mh
  Date:      31-Mrz-2003
  Arguments: Sender: TObject
  Result:    None
-----------------------------------------------------------------------------}

procedure TfrmMain.chkOfficeStyleClick(Sender: TObject);
begin
  styleOffice.Theme := TJvXPTheme(chkOfficeStyle.Checked);
end;

end.
