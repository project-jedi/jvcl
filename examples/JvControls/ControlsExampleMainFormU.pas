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

unit ControlsExampleMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvComCtrls, ExtCtrls, StdCtrls, JvCtrls, ImgList, JvStatusBar,
  JvListBox, JvButton, JvExStdCtrls, JvExComCtrls;

type
  TControlsExampleMainForm = class(TForm)
    JvStatusBar1: TJvStatusBar;
    Timer1: TTimer;
    JvPageControl1: TJvPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Bevel1: TBevel;
    JvTrackBar1: TJvTrackBar;
    Label3: TLabel;
    Button1: TButton;
    JvImgBtn1: TJvImgBtn;
    ImageList1: TImageList;
    JvImgBtn2: TJvImgBtn;
    JvImgBtn5: TJvImgBtn;
    JvImgBtn3: TJvImgBtn;
    JvImgBtn4: TJvImgBtn;
    Button2: TButton;
    JvImgBtn6: TJvImgBtn;
    JvImgBtn7: TJvImgBtn;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Bevel2: TBevel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvTrackBar1ToolTip(Sender: TObject; var ToolTipText: String);
    procedure JvListBox1GetText(Sender: TWinControl; Index: Integer; var Text: String);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure JvImgBtn6MouseEnter(Sender: TObject);
    procedure JvImgBtn6MouseLeave(Sender: TObject);
    procedure JvImgBtn7GetAnimateIndex(Sender: TObject; CurrentAnimateFrame: Byte; var ImageIndex: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  end;

var
  ControlsExampleMainForm: TControlsExampleMainForm;

implementation

{$R *.dfm}

procedure TControlsExampleMainForm.FormCreate(Sender: TObject);
begin
  Timer1Timer(nil);
  JvTrackBar1.Position := 20;
end;

procedure TControlsExampleMainForm.Timer1Timer(Sender: TObject);
begin
  JvStatusBar1.Panels[1].Text := TimeToStr(Now);
end;

procedure TControlsExampleMainForm.JvTrackBar1ToolTip(Sender: TObject; var ToolTipText: String);
begin
  ToolTipText := Format('TJvListBox displays %d items now', [JvTrackBar1.Position]);
end;

procedure TControlsExampleMainForm.JvListBox1GetText(Sender: TWinControl; Index: Integer;
  var Text: String);
begin
  Text := Format('Item no.: %d', [Index]);
end;

procedure TControlsExampleMainForm.Button1Click(Sender: TObject);
begin
  JvPageControl1.ActivePageIndex := 1;
end;

procedure TControlsExampleMainForm.Button2Click(Sender: TObject);
begin
  JvPageControl1.ActivePageIndex := 0;
end;

procedure TControlsExampleMainForm.JvImgBtn6MouseEnter(Sender: TObject);
begin
  with TJvImgBtn(Sender) do
  begin
    Color := clRed;
    Font.Style := [fsBold];
  end;
end;

procedure TControlsExampleMainForm.JvImgBtn6MouseLeave(Sender: TObject);
begin
  with TJvImgBtn(Sender) do
  begin
    Color := clBtnFace;
    Font.Style := [];
  end;  
end;

procedure TControlsExampleMainForm.JvImgBtn7GetAnimateIndex(Sender: TObject;
  CurrentAnimateFrame: Byte; var ImageIndex: Integer);
begin
  ImageIndex := CurrentAnimateFrame * 2 + 2;
end;

procedure TControlsExampleMainForm.RadioGroup1Click(Sender: TObject);
begin
  JvImgBtn4.Alignment := TAlignment(RadioGroup1.ItemIndex);
end;

procedure TControlsExampleMainForm.RadioGroup2Click(Sender: TObject);
begin
  JvImgBtn4.Layout := TJvImgBtnLayout(RadioGroup2.ItemIndex); 
end;

end.
