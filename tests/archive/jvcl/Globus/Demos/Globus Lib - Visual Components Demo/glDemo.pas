unit glDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, glPage, StdCtrls, glLBox, glALbox, glLabel, glGrBox, glBevel,
  glTView, ExtCtrls, glJump, Buttons, glTab, glImage, glFlyTxt, glDigits,
  glTypes, glCapt, glShape, glHint, ImgList, glHShape, glRuler, glGHC,
  Grids, DBGrids, glBitBtn, glSplit, glSGrid, glCBox, glSpeedButton,
  glSmallFontsDefence, glShadow, glWizardHeader, Mask, glEdit, glPrgrs,
  Spin;

type
  TForm1 = class(TForm)
    pcMain: TglPageControl;
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
    FrJumpingComponent1: TglJumpingComponent;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    glFlyingText: TglFlyingText;
    TabSheet12: TTabSheet;
    TabSheet13: TTabSheet;
    Image2: TImage;
    FrAskListBox1: TglAskListBox;
    FrBevel1: TglBevel;
    FrAskListBox2: TglAskListBox;
    HeaderControl1: THeaderControl;
    MainPageControlIL: TImageList;
    ListBox2IL: TImageList;
    ListBox1IL: TImageList;
    FrLabel4: TglLabel;
    FrLabel2: TglLabel;
    FrLabel3: TglLabel;
    Image3: TImage;
    glListBox1: TglListBox;
    glLabel2: TglLabel;
    TabSheet14: TTabSheet;
    glHint1: TglHint;
    Timer1: TTimer;
    glBevel1: TglBevel;
    glBevel2: TglBevel;
    glBevel3: TglBevel;
    glBevel4: TglBevel;
    glDigits2: TglDigits;
    glGroupBox4: TglGroupBox;
    Shape1: TShape;
    glBevel5: TglBevel;
    TabSheet15: TTabSheet;
    glRuler1: TglRuler;
    glRuler2: TglRuler;
    TabSheet16: TTabSheet;
    HeaderControl2: THeaderControl;
    FrCaption1: TglCaption;
    glBitBtn1: TglBitBtn;
    glBitBtn2: TglBitBtn;
    glBitBtn3: TglBitBtn;
    glBitBtn4: TglBitBtn;
    glStaticText1: TglStaticText;
    glLabel3: TglLabel;
    glSplitter1: TglSplitter;
    glBevel6: TglBevel;
    glBevel7: TglBevel;
    glHoleShape1: TglHoleShape;
    Panel2: TPanel;
    glLabel4: TglLabel;
    Label1: TLabel;
    glBevel8: TglBevel;
    glGroupBox5: TglGroupBox;
    glSpeedButton1: TglSpeedButton;
    glSpeedButton2: TglSpeedButton;
    glSpeedButton3: TglSpeedButton;
    ImageList1: TImageList;
    glSplitter2: TglSplitter;
    ShapeColor: TShape;
    glTabControl2: TglTabControl;
    glTabControl1: TglTabControl;
    ScrollBox1: TScrollBox;
    glGroupBox2a: TglGroupBox;
    glGroupBox1a: TglGroupBox;
    glGroupBox2: TglGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    glSpeedButton4: TglSpeedButton;
    glSmallFontsDefence1: TglSmallFontsDefence;
    TabSheet17: TTabSheet;
    glWizardHeader1: TglWizardHeader;
    glWizardHeader2: TglWizardHeader;
    glWizardHeader3: TglWizardHeader;
    glShadow1: TglShadow;
    Edit1: TEdit;
    glShadow2: TglShadow;
    Memo1: TMemo;
    TabSheet18: TTabSheet;
    glProgress1: TglProgress;
    glProgress2: TglProgress;
    glProgress3: TglProgress;
    TrackBar: TTrackBar;
    glProgress5: TglProgress;
    glProgress6: TglProgress;
    glProgress7: TglProgress;
    glProgress8: TglProgress;
    glProgress4: TglProgress;
    TabSheet19: TTabSheet;
    Image1: TImage;
    Image4: TImage;
    Image5: TImage;
    Panel1: TPanel;
    glListBox2: TglListBox;
    glListBox3: TglListBox;
    glLabel26: TglLabel;
    Image6: TImage;
    Shape2: TShape;
    glBitmapImage1: TglBitmapImage;
    Panel7: TPanel;
    glCheckTreeView1: TglCheckTreeView;
    tvInstallDemo: TglCheckTreeView;
    glSplitter3: TglSplitter;
    lCite: TglLabel;
    TabSheet20: TTabSheet;
    glCheckBox1: TglCheckBox;
    glCheckBox2: TglCheckBox;
    glCheckBox3: TglCheckBox;
    glCheckBox6: TglCheckBox;
    glCheckBox7: TglCheckBox;
    glGroupBox1: TglGroupBox;
    glCheckBox8: TglCheckBox;
    glCheckBox9: TglCheckBox;
    glCheckBox10: TglCheckBox;
    glCheckBox11: TglCheckBox;
    glCheckBox12: TglCheckBox;
    glCheckBox13: TglCheckBox;
    glCheckBox14: TglCheckBox;
    glCheckBox15: TglCheckBox;
    glGroupBox3: TglGroupBox;
    glCheckBox16: TglCheckBox;
    glCheckBox17: TglCheckBox;
    glCheckBox18: TglCheckBox;
    glCheckBox19: TglCheckBox;
    glDigits12: TglDigits;
    diTime: TglDigits;
    Timer2: TTimer;
    Panel6: TPanel;
    glGroupBox6: TglGroupBox;
    FrLabel16: TglLabel;
    FrLabel15: TglLabel;
    FrLabel17: TglLabel;
    Panel19: TPanel;
    AreaXSizeD: TglDigits;
    SpinButton1: TSpinButton;
    Panel16: TPanel;
    AreaYSizeD: TglDigits;
    SpinButton2: TSpinButton;
    Panel17: TPanel;
    AreaZSizeD: TglDigits;
    SpinButton3: TSpinButton;
    glGroupBox9: TglGroupBox;
    RzLabel1: TLabel;
    RzLabel7: TLabel;
    Label5: TLabel;
    TotDocsFrD: TglDigits;
    glDigits1: TglDigits;
    BDCountFrD: TglDigits;
    glGroupBox10: TglGroupBox;
    glLabel14: TglLabel;
    glLabel19: TglLabel;
    glLabel20: TglLabel;
    Panel8: TPanel;
    glDigits4: TglDigits;
    SpinButton4: TSpinButton;
    Panel9: TPanel;
    glDigits13: TglDigits;
    SpinButton5: TSpinButton;
    Panel10: TPanel;
    glDigits14: TglDigits;
    SpinButton6: TSpinButton;
    Edit2: TEdit;
    glShadow3: TglShadow;
    glShadow4: TglShadow;
    glBevel9: TglBevel;
    Memo2: TMemo;
    glShadow5: TglShadow;
    glShadow6: TglShadow;
    Memo4: TMemo;
    glCheckTreeView2: TglCheckTreeView;
    glShadow7: TglShadow;
    SpeedButton1: TSpeedButton;
    glShadow8: TglShadow;
    SpeedButton2: TSpeedButton;
    glShadow9: TglShadow;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    glShadow10: TglShadow;
    glShadow11: TglShadow;
    glShadow12: TglShadow;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    glShadow13: TglShadow;
    glShadow14: TglShadow;
    glShadow15: TglShadow;
    glGroupBox3a: TglGroupBox;
    Shape3: TShape;
    glGroupBox11: TglGroupBox;
    Shape4: TShape;
    glLabel21: TglLabel;
    glLabel22: TglLabel;
    glListBox4: TglListBox;
    Image7: TImage;
    glCheckListBox1: TglCheckListBox;
    ImageList2: TImageList;
    glAskListBox1: TglAskListBox;
    Image8: TImage;
    glStaticText2: TglStaticText;
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
    procedure Timer2Timer(Sender: TObject);
    procedure glFlyingTextTextLineChanging(Sender: TObject;
      LineNum: Integer);
    procedure sbTglHelpPanelClick(Sender: TObject);
  private
    CanvasBmp,ShowBmp: TBitmap;
    yShift,yShiftHeight,yShift_,yShiftHeight_: integer;
    sStr: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses glUtils, HShape, shellApi, glHelpPanel_demo;
{$R *.DFM}

procedure TForm1.pcMainChange(Sender: TObject);
begin
  with (sender as TglPageControl) do
  begin
    glFlyingText.Active := ActivePage = TabSheet9;
    FrJumpingComponent1.Enabled := ActivePage=TabSheet5;
    Timer1.Enabled:=ActivePage=TabSheet12;
  end;
end;

procedure TForm1.FreeFlyingText1TextLineChanging(Sender: TObject;
  LineNum: Integer);
begin
//  with FreeFlyingText1 do
//  if Direction=fsdRaising then Direction:=fsdRecessing
//  else Direction:=fsdRaising;
end;

procedure TForm1.sbShowHoleShapeDemoClick(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.glTabControl3GetItemColor(Sender: TObject; Index: Integer;
  var Color: TColor);
begin
  if Index = (Sender as TglTabControl).TabIndex then
  begin
    Color := RGB( 128 + Random(128), 128 + Random(128), 128 + Random(128));
    ShapeColor.Brush.Color := Color;
  end;
end;

procedure TForm1.TrackBarChange(Sender: TObject);
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

procedure TForm1.FormActivate(Sender: TObject);
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

procedure TForm1.lCiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', Pchar(lCite.Caption), '', '', SW_SHOW);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
var
  Hour, Min, Sec, MSec: Word;
begin
//  DecodeTime(Time);
//  diTime.Value := Hour
end;

procedure TForm1.glFlyingTextTextLineChanging(Sender: TObject; LineNum: Integer);
begin
  with glFlyingText do
  begin
    Gradient.FromColor := RGB(random(100),random(100),random(100));
    Gradient.ToColor := IncColor(Gradient.FromColor, 100);
//    Gradient3D.ToColor := IncColor(Gradient.FromColor, 200);
  end;
end;

procedure TForm1.sbTglHelpPanelClick(Sender: TObject);
begin
  fTglHelpPanel.ShowModal;
end;

end.
