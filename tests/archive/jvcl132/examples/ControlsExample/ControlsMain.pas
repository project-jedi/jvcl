unit ControlsMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, JvComCtrls, ExtCtrls, StdCtrls, JvCtrls, ImgList, JvStatusBar;

type
  TMainFrom = class(TForm)
    JvStatusBar1: TJvStatusBar;
    Timer1: TTimer;
    JvPageControl1: TJvPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Bevel1: TBevel;
    JvListBox1: TJvListBox;
    Label2: TLabel;
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
    procedure JvTrackBar1Change(Sender: TObject);
    procedure JvListBox1GetText(Sender: TWinControl; Index: Integer;
      var Text: String);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure JvImgBtn6MouseEnter(Sender: TObject);
    procedure JvImgBtn6MouseLeave(Sender: TObject);
    procedure JvImgBtn7GetAnimateIndex(Sender: TObject;
      CurrentAnimateFrame: Byte; var ImageIndex: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainFrom: TMainFrom;

implementation

{$R *.DFM}

procedure TMainFrom.FormCreate(Sender: TObject);
begin
  Timer1Timer(nil);
  JvTrackBar1.Position := 20;
end;

procedure TMainFrom.Timer1Timer(Sender: TObject);
begin
  JvStatusBar1.Panels[1].Text := TimeToStr(Now);
end;


procedure TMainFrom.JvTrackBar1ToolTip(Sender: TObject; var ToolTipText: String);
begin
  ToolTipText := Format('TJvListBox displays %d items now', [JvTrackBar1.Position]);
end;

procedure TMainFrom.JvTrackBar1Change(Sender: TObject);
begin
  JvListBox1.Count := JvTrackBar1.Position;
end;

procedure TMainFrom.JvListBox1GetText(Sender: TWinControl; Index: Integer;
  var Text: String);
begin
  Text := Format('Item no.: %d', [Index]);
end;

procedure TMainFrom.Button1Click(Sender: TObject);
begin
  JvPageControl1.ActivePageIndex := 1;
end;

procedure TMainFrom.Button2Click(Sender: TObject);
begin
  JvPageControl1.ActivePageIndex := 0;
end;

procedure TMainFrom.JvImgBtn6MouseEnter(Sender: TObject);
begin
  with TJvImgBtn(Sender) do
  begin
    Color := clRed;
    Font.Style := [fsBold];
  end;
end;

procedure TMainFrom.JvImgBtn6MouseLeave(Sender: TObject);
begin
  with TJvImgBtn(Sender) do
  begin
    Color := clBtnFace;
    Font.Style := [];
  end;  
end;

procedure TMainFrom.JvImgBtn7GetAnimateIndex(Sender: TObject;
  CurrentAnimateFrame: Byte; var ImageIndex: Integer);
begin
  ImageIndex := CurrentAnimateFrame * 2 + 2;
end;

procedure TMainFrom.RadioGroup1Click(Sender: TObject);
begin
  JvImgBtn4.Alignment := TAlignment(RadioGroup1.ItemIndex);
end;

procedure TMainFrom.RadioGroup2Click(Sender: TObject);
begin
  JvImgBtn4.Layout := TJvImgBtnLayout(RadioGroup2.ItemIndex); 
end;

end.
