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
unit JvOutlookBarCustomDrawDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, JvExControls, JvComponent, JvOutlookBar,
  JvNavigationPane, StdCtrls;

type
  TJvOutlookBarCustomDrawDemoMainFrm = class(TForm)
    JvOutlookBar1: TJvOutlookBar;
    ImageList1: TImageList;
    ImageList2: TImageList;
    JvNavPaneStyleManager1: TJvNavPaneStyleManager;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  public
    { Public declarations }
    procedure DoCustomDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
    AStage: TJvOutlookBarCustomDrawStage; AIndex:integer; ADown, AInside: boolean;
    var DefaultDraw:boolean);
  end;

var
  JvOutlookBarCustomDrawDemoMainFrm: TJvOutlookBarCustomDrawDemoMainFrm;

implementation

uses
  JvJVCLUtils;

{$R *.dfm}

procedure TJvOutlookBarCustomDrawDemoMainFrm.DoCustomDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AStage: TJvOutlookBarCustomDrawStage; AIndex: integer; ADown,  AInside: boolean; var DefaultDraw:boolean);
begin
  DefaultDraw := False;
  case AStage of
  odsBackground:
     with JvNavPaneStyleManager1.Colors do
       GradientFillRect(ACanvas, ARect, HeaderColorFrom, HeaderColorTo, fdTopToBottom, 255);
  odsPage:
     with JvNavPaneStyleManager1.Colors do
       GradientFillRect(ACanvas,ARect, ButtonColorFrom, ButtonColorTo, fdTopToBottom, 255);
  odsPageButton:
  begin
     with JvNavPaneStyleManager1.Colors do
       GradientFillRect(ACanvas,ARect, HeaderColorFrom, HeaderColorTo, fdTopToBottom, 255);
     if ADown then
       OffsetRect(ARect,1,1);
     ACanvas.Font.Color := clWhite;
     DrawText(ACanvas.Handle, PChar(JvOutlookBar1.Pages[AIndex].Caption),
       Length(JvOutlookBar1.Pages[AIndex].Caption), ARect, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
  end;
  odsButtonFrame:
  begin
    if ADown then
      ACanvas.Brush.Color := clNavy
    else
      ACanvas.Brush.Color := clBlack;
    ACanvas.FrameRect(ARect);
    InflateRect(ARect,-1,-1);
    if not ADown then
      ACanvas.Brush.Color := clWhite;
    ACanvas.FrameRect(ARect);
  end;
  odsButton:
    DefaultDraw := True;
  end;
end;

procedure TJvOutlookBarCustomDrawDemoMainFrm.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := 0;
  JvOutlookBar1.OnCustomDraw := DoCustomDraw;
  ComboBox2.ItemIndex := 0;
  ComboBox1Change(ComboBox1);
  ComboBox2Change(ComboBox2);
end;

procedure TJvOutlookBarCustomDrawDemoMainFrm.ComboBox1Change(Sender: TObject);
begin
  JvNavPaneStyleManager1.Theme := TJvNavPanelTheme(ComboBox1.ItemIndex);
  JvOutlookBar1.Invalidate;
end;

procedure TJvOutlookBarCustomDrawDemoMainFrm.ComboBox2Change(Sender: TObject);
begin
  JvOutlookBar1.ButtonSize := TJvBarButtonSize(ComboBox2.ItemIndex);
end;

end.
