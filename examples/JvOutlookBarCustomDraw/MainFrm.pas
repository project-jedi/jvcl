unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, JvExControls, JvComponent, JvOutlookBar,
  JvNavigationPane, StdCtrls;

type
  TForm1 = class(TForm)
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
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoCustomDraw(Sender:TObject; ACanvas:TCanvas; ARect:TRect;
    AStage:TJvOutlookBarCustomDrawStage; AIndex:integer; ADown, AInside:boolean; var DefaultDraw:boolean);
  end;

var
  Form1: TForm1;

implementation
uses
  JvJVCLUtils;

{$R *.dfm}

procedure TForm1.DoCustomDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AStage: TJvOutlookBarCustomDrawStage; AIndex: integer; ADown,  AInside: boolean; var DefaultDraw:boolean);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := 0;
  JvOutlookBar1.OnCustomDraw := DoCustomDraw;
  ComboBox2.ItemIndex := 0;
  ComboBox1Change(ComboBox1);
  ComboBox2Change(ComboBox2);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  JvNavPaneStyleManager1.Theme := TJvNavPanelTheme(ComboBox1.ItemIndex);
  JvOutlookBar1.Invalidate;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  JvOutlookBar1.ButtonSize := TJvBarButtonSize(ComboBox2.ItemIndex);
end;

end.
