unit ExampleJVSegmentedLEDDisplay16SegMain;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, JvComponent, JvLEDDisplays, Buttons;

type
  Tfme16SegExamples = class(TFrame)
    pcSeg16: TPageControl;
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
    cxA1: TCheckBox;
    cxB: TCheckBox;
    cxC: TCheckBox;
    cxD1: TCheckBox;
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
    cxA2: TCheckBox;
    cxD2: TCheckBox;
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

uses
  JclBase;

var
  sMainCharSet: string = '0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ ';
  AltSetSegs: TDynStringArray;
  AltSegPos: Integer = 0;

procedure Tfme16SegExamples.ScrollMainCharset;

  procedure SetAltSegSegments;
  var
    IDigit: Integer;
    ISegPos: Integer;
  begin
    IDigit := 0;
    ISegPos := AltSegPos;
    while IDigit < sldCharsetSpec.DigitCount do
    begin
      sldCharsetSpec.Digits[IDigit].Segments := AltSetSegs[ISegPos];
      Inc(IDigit);
      Inc(ISegPos);
      if ISegPos > High(AltSetSegs) then
        ISegPos := 0;
    end;
  end;
  
begin
  Inc(ScrCnt);
  if not Odd(ScrCnt) then
  begin
    sMainCharSet := Copy(sMainCharSet, 2, Length(sMainCharSet) - 1) + sMainCharSet[1];
    sldCharsetMain.Text := sMainCharSet;
    Inc(AltSegPos);
    if AltSegPos > High(AltSetSegs) then
      AltSegPos := 0;
    SetAltSegSegments;
  end;
end;

procedure Tfme16SegExamples.UpdateCheckMarks;
var
  S: string;
begin
  S := sldTest.Digits[0].Segments;
  cxA1.Checked := Pos('A1', S) > 0;
  cxA2.Checked := Pos('A2', S) > 0;
  cxB.Checked := Pos('B', S) > 0;
  cxC.Checked := Pos('C', S) > 0;
  cxD1.Checked := Pos('D1', S) > 0;
  cxD2.Checked := Pos('D2', S) > 0;
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

procedure Tfme16SegExamples.Loaded;
begin
  inherited Loaded;
  pcSeg16.ActivePageIndex := 0;
  AltSetSegs := sldTest.DigitStringToSegments('[#0][#4] -+*()°''"/\,%&.:[[]{} ');
  edSegString.Text := 'ABCDEFGHJKLMNDP';
  btnSetSegs.Click;
end;

procedure Tfme16SegExamples.TimerEvent;
begin
  ScrollMainCharset;
end;

procedure Tfme16SegExamples.CheckMarkClicked(Sender: TObject);
var
  S: string;
begin
  if not InCheckMarkChanging and not InCharacterSetting and not InSegmentSetting then
  try
    InCheckMarkChanging := True;
    S := '';
    if cxA1.Checked then S := S + 'A1';
    if cxA2.Checked then S := S + 'A2';
    if cxB.Checked then S := S + 'B';
    if cxC.Checked then S := S + 'C';
    if cxD1.Checked then S := S + 'D1';
    if cxD2.Checked then S := S + 'D2';
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

procedure Tfme16SegExamples.btnSetCharClick(Sender: TObject);
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

procedure Tfme16SegExamples.btnSetSegsClick(Sender: TObject);
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
