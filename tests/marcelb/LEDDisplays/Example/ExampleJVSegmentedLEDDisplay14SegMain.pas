unit ExampleJVSegmentedLEDDisplay14SegMain;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, JvComponent, JvLEDDisplays, Buttons;

type
  Tfme14SegExamples = class(TFrame)
    pcSeg14: TPageControl;
    tsInfo: TTabSheet;
    lblInfoCaption: TLabel;
    lblDescription: TLabel;
    lblCharsetCaption: TLabel;
    lblCharsetMain: TLabel;
    sldCharsetMain: TJvSegmentLEDDisplay;
    lblCharsetSpec: TLabel;
    sldCharsetSpec: TJvSegmentLEDDisplay;
    tsTestArea: TTabSheet;
    sldTest: TJvSegmentLEDDisplay;
    cxA: TCheckBox;
    cxB: TCheckBox;
    cxC: TCheckBox;
    cxD: TCheckBox;
    cxE: TCheckBox;
    cxF: TCheckBox;
    cxG1: TCheckBox;
    cxG2: TCheckBox;
    cxH: TCheckBox;
    cxJ: TCheckBox;
    cxK: TCheckBox;
    cxL: TCheckBox;
    cxM: TCheckBox;
    cxN: TCheckBox;
    cxDP: TCheckBox;
    lblCharacter: TLabel;
    edCharacter: TEdit;
    lblSegString: TLabel;
    edSegString: TEdit;
    btnSetChar: TSpeedButton;
    btnSetSegs: TSpeedButton;
    procedure CheckMarkClicked(Sender: TObject);
    procedure btnSetCharClick(Sender: TObject);
    procedure btnSetSegsClick(Sender: TObject);
  private
    { Private declarations }
    ScrCnt: Cardinal;
    InCheckMarkChanging: Boolean;
    InCharacterSetting: Boolean;
    InSegmentSetting: Boolean;
    procedure ScrollMainCharset;
    procedure UpdateCheckMarks;
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    procedure TimerEvent;
  end;

implementation

{$R *.DFM}

var
  sMainCharSet: string = '0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ ';

procedure Tfme14SegExamples.ScrollMainCharset;
begin
  Inc(ScrCnt);
  if not Odd(ScrCnt) then
  begin
    sMainCharSet := Copy(sMainCharSet, 2, Length(sMainCharSet) - 1) + sMainCharSet[1];
    sldCharsetMain.Text := sMainCharSet;
  end;
end;

procedure Tfme14SegExamples.UpdateCheckMarks;
var
  S: string;
begin
  S := sldTest.Digits[0].Segments;
  cxA.Checked := Pos('A', S) > 0;
  cxB.Checked := Pos('B', S) > 0;
  cxC.Checked := Pos('C', S) > 0;
  cxD.Checked := Pos('D', S) > 0;
  cxE.Checked := Pos('E', S) > 0;
  cxF.Checked := Pos('F', S) > 0;
  cxG1.Checked := Pos('G1', S) > 0;
  cxG2.Checked := Pos('G2', S) > 0;
  cxH.Checked := Pos('H', S) > 0;
  cxJ.Checked := Pos('J', S) > 0;
  cxK.Checked := Pos('K', S) > 0;
  cxL.Checked := Pos('L', S) > 0;
  cxM.Checked := Pos('M', S) > 0;
  cxN.Checked := Pos('N', S) > 0;
  cxDP.Checked := Pos('DP', S) > 0;
end;

procedure Tfme14SegExamples.Loaded;
begin
  inherited Loaded;
  pcSeg14.ActivePageIndex := 0;
end;

procedure Tfme14SegExamples.TimerEvent;
begin
  ScrollMainCharset;
end;

procedure Tfme14SegExamples.CheckMarkClicked(Sender: TObject);
var
  S: string;
begin
  if not InCheckMarkChanging and not InCharacterSetting and not InSegmentSetting then
  try
    InCheckMarkChanging := True;
    S := '';
    if cxA.Checked then S := S + 'A';
    if cxB.Checked then S := S + 'B';
    if cxC.Checked then S := S + 'C';
    if cxD.Checked then S := S + 'D';
    if cxE.Checked then S := S + 'E';
    if cxF.Checked then S := S + 'F';
    if cxG1.Checked then S := S + 'G1';
    if cxG2.Checked then S := S + 'G2';
    if cxH.Checked then S := S + 'H';
    if cxJ.Checked then S := S + 'J';
    if cxK.Checked then S := S + 'K';
    if cxL.Checked then S := S + 'L';
    if cxM.Checked then S := S + 'M';
    if cxN.Checked then S := S + 'N';
    if cxDP.Checked then S := S + 'DP';
    sldTest.Digits[0].Segments := S;
    edCharacter.Text := '';
    edSegString.Text := sldTest.Digits[0].Segments;
  finally
    InCheckMarkChanging := False;
  end;
end;

procedure Tfme14SegExamples.btnSetCharClick(Sender: TObject);
begin
  InCharacterSetting := True;
  try
    sldTest.Text := edCharacter.Text;
    edSegString.Text := sldTest.Digits[0].Segments;
    UpdateCheckMarks;
  finally
    InCharacterSetting := False;
  end;
end;

procedure Tfme14SegExamples.btnSetSegsClick(Sender: TObject);
begin
  InSegmentSetting := True;
  try
    sldTest.Digits[0].Segments := edSegString.Text;
    edSegString.Text := sldTest.Digits[0].Segments;
    UpdateCheckMarks;
  finally
    InSegmentSetting := False;
  end;
end;

end.
