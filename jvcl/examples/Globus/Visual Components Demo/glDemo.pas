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

unit glDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvgPage, StdCtrls, JvgListBox, JvgAskListBox, JvgLabel, JvgGroupBox, JvgBevel,
  JvgTreeView, ExtCtrls, JvgJump, Buttons, JvgTab, JvgImage, JvgFlyingText, JvgDigits,
  JvgTypes, JvgCaption, JvgShape, JvgHint, ImgList, JvgHoleShape, JvgRuler, JvgGridHeaderControl,
  Grids, DBGrids, JvgBitBtn, JvgSplit, JvgStringGrid, JvgCheckBox, JvgSpeedButton,
  JvgShadow, JvgWizardHeader, Mask, JvgEdit, JvgProgress,
  Spin, JvComponent, JvgStaticText, JvgSmallFontsDefense, JvExButtons,
  JvExExtCtrls, JvExComCtrls, JvExControls;

type
  TGLDemoFrm = class(TForm)
    pcMain: TJvgPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    FrJumpingComponent1: TJvgJumpingComponent;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    glFlyingText: TJvgFlyingText;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    Image2: TImage;
    FrAskListBox1: TJvgAskListBox;
    FrBevel1: TJvgBevel;
    FrAskListBox2: TJvgAskListBox;
    HeaderControl1: THeaderControl;
    MainPageControlIL: TImageList;
    ListBox2IL: TImageList;
    ListBox1IL: TImageList;
    FrLabel4: TJvgLabel;
    FrLabel2: TJvgLabel;
    FrLabel3: TJvgLabel;
    Image3: TImage;
    glListBox1: TJvgListBox;
    glLabel2: TJvgLabel;
    TabSheet14: TTabSheet;
    glHint1: TJvgHint;
    Timer1: TTimer;
    glBevel1: TJvgBevel;
    glBevel2: TJvgBevel;
    glBevel3: TJvgBevel;
    glBevel4: TJvgBevel;
    glDigits2: TJvgDigits;
    glGroupBox4: TJvgGroupBox;
    Shape1: TShape;
    glBevel5: TJvgBevel;
    TabSheet15: TTabSheet;
    glRuler1: TJvgRuler;
    glRuler2: TJvgRuler;
    TabSheet16: TTabSheet;
    HeaderControl2: THeaderControl;
    FrCaption1: TJvgCaption;
    glBitBtn1: TJvgBitBtn;
    glBitBtn2: TJvgBitBtn;
    glBitBtn3: TJvgBitBtn;
    glBitBtn4: TJvgBitBtn;
    glStaticText1: TJvgStaticText;
    glLabel3: TJvgLabel;
    glSplitter1: TJvgSplitter;
    glBevel6: TJvgBevel;
    glBevel7: TJvgBevel;
    glHoleShape1: TJvgHoleShape;
    Panel2: TPanel;
    glLabel4: TJvgLabel;
    Label1: TLabel;
    glBevel8: TJvgBevel;
    glGroupBox5: TJvgGroupBox;
    glSpeedButton1: TJvgSpeedButton;
    glSpeedButton2: TJvgSpeedButton;
    glSpeedButton3: TJvgSpeedButton;
    ImageList1: TImageList;
    glSplitter2: TJvgSplitter;
    ShapeColor: TShape;
    glTabControl2: TJvgTabControl;
    glTabControl1: TJvgTabControl;
    ScrollBox1: TScrollBox;
    glGroupBox2a: TJvgGroupBox;
    glGroupBox1a: TJvgGroupBox;
    glGroupBox2: TJvgGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    glSpeedButton4: TJvgSpeedButton;
    TabSheet17: TTabSheet;
    glWizardHeader1: TJvgWizardHeader;
    glWizardHeader2: TJvgWizardHeader;
    glWizardHeader3: TJvgWizardHeader;
    glShadow1: TJvgShadow;
    Edit1: TEdit;
    glShadow2: TJvgShadow;
    Memo1: TMemo;
    TabSheet18: TTabSheet;
    glProgress1: TJvgProgress;
    glProgress2: TJvgProgress;
    glProgress3: TJvgProgress;
    TrackBar: TTrackBar;
    glProgress5: TJvgProgress;
    glProgress6: TJvgProgress;
    glProgress7: TJvgProgress;
    glProgress8: TJvgProgress;
    glProgress4: TJvgProgress;
    TabSheet19: TTabSheet;
    Image1: TImage;
    Image4: TImage;
    Image5: TImage;
    Panel1: TPanel;
    glListBox2: TJvgListBox;
    glListBox3: TJvgListBox;
    glLabel26: TJvgLabel;
    Image6: TImage;
    Shape2: TShape;
    glBitmapImage1: TJvgBitmapImage;
    Panel7: TPanel;
    glCheckTreeView1: TJvgCheckTreeView;
    tvInstallDemo: TJvgCheckTreeView;
    glSplitter3: TJvgSplitter;
    lCite: TJvgLabel;
    TabSheet20: TTabSheet;
    glCheckBox1: TJvgCheckBox;
    glCheckBox2: TJvgCheckBox;
    glCheckBox3: TJvgCheckBox;
    glCheckBox6: TJvgCheckBox;
    glCheckBox7: TJvgCheckBox;
    glGroupBox1: TJvgGroupBox;
    glCheckBox8: TJvgCheckBox;
    glCheckBox9: TJvgCheckBox;
    glCheckBox10: TJvgCheckBox;
    glCheckBox11: TJvgCheckBox;
    glCheckBox12: TJvgCheckBox;
    glCheckBox13: TJvgCheckBox;
    glCheckBox14: TJvgCheckBox;
    glCheckBox15: TJvgCheckBox;
    glGroupBox3: TJvgGroupBox;
    glCheckBox16: TJvgCheckBox;
    glCheckBox17: TJvgCheckBox;
    glCheckBox18: TJvgCheckBox;
    glCheckBox19: TJvgCheckBox;
    glDigits12: TJvgDigits;
    diTime: TJvgDigits;
    Panel6: TPanel;
    glGroupBox6: TJvgGroupBox;
    FrLabel16: TJvgLabel;
    FrLabel15: TJvgLabel;
    FrLabel17: TJvgLabel;
    Panel19: TPanel;
    AreaXSizeD: TJvgDigits;
    SpinButton1: TSpinButton;
    Panel16: TPanel;
    AreaYSizeD: TJvgDigits;
    SpinButton2: TSpinButton;
    Panel17: TPanel;
    AreaZSizeD: TJvgDigits;
    SpinButton3: TSpinButton;
    glGroupBox9: TJvgGroupBox;
    RzLabel1: TLabel;
    RzLabel7: TLabel;
    Label5: TLabel;
    TotDocsFrD: TJvgDigits;
    glDigits1: TJvgDigits;
    BDCountFrD: TJvgDigits;
    glGroupBox10: TJvgGroupBox;
    glLabel14: TJvgLabel;
    glLabel19: TJvgLabel;
    glLabel20: TJvgLabel;
    Panel8: TPanel;
    glDigits4: TJvgDigits;
    SpinButton4: TSpinButton;
    Panel9: TPanel;
    glDigits13: TJvgDigits;
    SpinButton5: TSpinButton;
    Panel10: TPanel;
    glDigits14: TJvgDigits;
    SpinButton6: TSpinButton;
    Edit2: TEdit;
    glShadow3: TJvgShadow;
    glShadow4: TJvgShadow;
    glBevel9: TJvgBevel;
    Memo2: TMemo;
    glShadow5: TJvgShadow;
    glShadow6: TJvgShadow;
    Memo4: TMemo;
    glCheckTreeView2: TJvgCheckTreeView;
    glShadow7: TJvgShadow;
    SpeedButton1: TSpeedButton;
    glShadow8: TJvgShadow;
    SpeedButton2: TSpeedButton;
    glShadow9: TJvgShadow;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    glShadow10: TJvgShadow;
    glShadow11: TJvgShadow;
    glShadow12: TJvgShadow;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    glShadow13: TJvgShadow;
    glShadow14: TJvgShadow;
    glShadow15: TJvgShadow;
    glGroupBox3a: TJvgGroupBox;
    Shape3: TShape;
    glGroupBox11: TJvgGroupBox;
    Shape4: TShape;
    glLabel21: TJvgLabel;
    glLabel22: TJvgLabel;
    glListBox4: TJvgListBox;
    glCheckListBox1: TJvgCheckListBox;
    ImageList2: TImageList;
    glAskListBox1: TJvgAskListBox;
    Image8: TImage;
    glStaticText2: TJvgStaticText;
    sbTglHelpPanel: TSpeedButton;
    procedure pcMainChange(Sender: TObject);
    procedure FreeFlyingText1TextLineChanging(Sender: TObject;
      LineNum: Integer);
    procedure sbShowHoleShapeDemoClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure glTabControl3GetItemColor(Sender: TObject; Index: Integer;
      var Color: TColor);
    procedure TrackBarChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lCiteClick(Sender: TObject);
    procedure glFlyingTextTextLineChanging(Sender: TObject;
      LineNum: Integer);
    procedure sbTglHelpPanelClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  GLDemoFrm: TGLDemoFrm;

implementation

uses JvgUtils, HShape, shellApi, glHelpPanel_demo;

{$R *.DFM}

procedure TGLDemoFrm.pcMainChange(Sender: TObject);
begin
  with (sender as TJvgPageControl) do
  begin
    glFlyingText.Active := ActivePage = TabSheet9;
    FrJumpingComponent1.Active := ActivePage = TabSheet5;
    Timer1.Enabled := ActivePage = TabSheet12;
  end;
end;

procedure TGLDemoFrm.FreeFlyingText1TextLineChanging(Sender: TObject;
  LineNum: Integer);
begin
//  with FreeFlyingText1 do
//  if Direction=fsdRaising then Direction:=fsdRecessing
//  else Direction:=fsdRaising;
end;

procedure TGLDemoFrm.sbShowHoleShapeDemoClick(Sender: TObject);
begin
  if not assigned(HShapeFrm) then
    HShapeFrm := THShapeFrm.create(Application);
  HShapeFrm.ShowModal;
end;

procedure TGLDemoFrm.Panel1Click(Sender: TObject);
begin
  close;
end;

procedure TGLDemoFrm.glTabControl3GetItemColor(Sender: TObject; Index: Integer;
  var Color: TColor);
begin
  if Index = (Sender as TJvgTabControl).TabIndex then
  begin
    Color := RGB(128 + Random(128), 128 + Random(128), 128 + Random(128));
    ShapeColor.Brush.Color := Color;
  end;
end;

procedure TGLDemoFrm.TrackBarChange(Sender: TObject);
begin
  glProgress1.Percent := TrackBar.Position;
  glProgress2.Percent := 100 - TrackBar.Position;
  glProgress3.Percent := TrackBar.Position;
  glProgress4.Percent := 100 - TrackBar.Position;
  glProgress5.Percent := TrackBar.Position;
  glProgress6.Percent := TrackBar.Position;
  glProgress7.Percent := TrackBar.Position;
  glProgress8.Percent := 100 - TrackBar.Position;
end;

procedure TGLDemoFrm.FormActivate(Sender: TObject);
var
  Node: TTreeNode;
begin
  // prepare TreeView
  Node := tvInstallDemo.Items[0];

  tvInstallDemo.SetNodeBoldState(Node, true);
  Node.Expand(true);
  tvInstallDemo.SetStateIndex(Node, ncsUndefined);

  Node := tvInstallDemo.Items[4];

  tvInstallDemo.SetNodeBoldState(Node, true);
  Node.Expand(true);
  tvInstallDemo.SetStateIndex(Node, ncsUndefined);
end;

procedure TGLDemoFrm.lCiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', Pchar(lCite.Caption), '', '', SW_SHOW);
end;

procedure TGLDemoFrm.glFlyingTextTextLineChanging(Sender: TObject; LineNum: Integer);
begin
  with glFlyingText do
  begin
    Gradient.FromColor := RGB(random(100), random(100), random(100));
    Gradient.ToColor := IncColor(Gradient.FromColor, 100);
//    Gradient3D.ToColor := IncColor(Gradient.FromColor, 200);
  end;
end;

procedure TGLDemoFrm.sbTglHelpPanelClick(Sender: TObject);
begin
  fTglHelpPanel.ShowModal;
end;

end.

