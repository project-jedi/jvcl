unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Memo2: TMemo;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    HIndices:array[0..31] of integer;
    HValues:array[0..255] of integer;
    AssoValues:array[0..255] of integer;
    intest:boolean;
    procedure FillIndices;
    function FillValues:integer;
    function TestHValue(const TokenStr: string): integer;
    function TestIndices:boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
uses JvInterpreterParser;

type
  TTokenTag = record
    // (rom) changed to PChar to get rid of hidden initialization section
    Token: PChar;
    TTyp: TTokenKind;
  end;

const
  WordList: array [0..51] of TTokenTag = (
    (Token: kwTRUE; TTyp: ttTrue),
    (Token: kwFALSE; TTyp: ttFalse),
    (Token: kwOR; TTyp: ttOr),
    (Token: kwAND; TTyp: ttAnd),
    (Token: kwNOT; TTyp: ttNot),
    (Token: kwDIV; TTyp: ttIntDiv),
    (Token: kwMOD; TTyp: ttMod),
    (Token: kwBEGIN; TTyp: ttBegin),
    (Token: kwEND; TTyp: ttEnd),
    (Token: kwIF; TTyp: ttIf),
    (Token: kwTHEN; TTyp: ttThen),
    (Token: kwELSE; TTyp: ttElse),
    (Token: kwWHILE; TTyp: ttWhile),
    (Token: kwDO; TTyp: ttDo),
    (Token: kwREPEAT; TTyp: ttRepeat),
    (Token: kwUNTIL; TTyp: ttUntil),
    (Token: kwPROCEDURE; TTyp: ttProcedure),
    (Token: kwFUNCTION; TTyp: ttFunction),
    (Token: kwFOR; TTyp: ttFor),
    (Token: kwTO; TTyp: ttTo),
    (Token: kwBREAK; TTyp: ttBreak),
    (Token: kwCONTINUE; TTyp: ttContinue),
    (Token: kwVAR; TTyp: ttVar),
    (Token: kwTRY; TTyp: ttTry),
    (Token: kwFINALLY; TTyp: ttFinally),
    (Token: kwEXCEPT; TTyp: ttExcept),
    (Token: kwON; TTyp: ttOn),
    (Token: kwRAISE; TTyp: ttRaise),
    (Token: kwEXTERNAL; TTyp: ttExternal),
    (Token: kwUNIT; TTyp: ttUnit),
    (Token: kwUSES; TTyp: ttUses),
    (Token: kwCONST; TTyp: ttConst),
    (Token: kwPUBLIC; TTyp: ttPublic),
    (Token: kwPRIVATE; TTyp: ttPrivate),
    (Token: kwPROTECTED; TTyp: ttProtected),
    (Token: kwPUBLISHED; TTyp: ttPublished),
    (Token: kwPROPERTY; TTyp: ttProperty),
    (Token: kwCLASS; TTyp: ttClass),
    (Token: kwTYPE; TTyp: ttType),
    (Token: kwINTERFACE; TTyp: ttInterface),
    (Token: kwIMPLEMENTATION; TTyp: ttImplementation),
    (Token: kwEXIT; TTyp: ttExit),
    (Token: kwARRAY; TTyp: ttArray),
    (Token: kwOF; TTyp: ttOf),
    (Token: kwCASE; TTyp: ttCase),
    (Token: kwPROGRAM; TTyp: ttProgram),
    (Token: kwIN; TTyp: ttIn),
    (Token: kwRECORD; TTyp: ttRecord),
    (Token: kwDOWNTO; TTyp: ttDownTo),
    (Token: kwSHL; TTyp: ttShl), // [peter schraut: added on 2005/08/14]
    (Token: kwSHR; TTyp: ttShr), // [peter schraut: added on 2005/08/14]
    (Token: kwXOR; TTyp: ttXor)  // [peter schraut: added on 2005/08/14]
    );

{ TForm1 }

procedure TForm1.FillIndices;
var
  i,temp:integer;
begin
  for i:=0 to High(HIndices) do
    //HIndices[i]:=random(64)*random(64) div 64;
    HIndices[i]:=random(100);
end;

function TForm1.FillValues:integer;
var
  i:integer;
  HIndex:integer;
begin
  for i:=0 to High(HValues) do
    HValues[i]:=-1;
  for result:=0 to High(WordList) do begin
    HIndex:=TestHValue(WordList[result].Token);
    if HValues[HIndex]=-1 then
      HValues[HIndex]:=result
    else
      Exit;
      //raise Exception.CreateFmt('Collision at step %d',[result]);
  end;
end;

function TForm1.TestHValue(const TokenStr: string): integer;
var
  Len,i: Integer;
  HVal: Integer;
begin
  Len := Length(TokenStr);

  begin
    HVal := Len;
    for i:=1 to Len do begin
      HVal := HVal + HIndices[(Byte(TokenStr[i]) - Byte('a')) and $1F];
      if i=3 then
        Break;
    end;
    HVal := HVal + HIndices[(Byte(TokenStr[Len]) - Byte('a')) and $1F];
    result := HVal and 255;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if intest then begin
    intest:=false;
    exit;
  end;
  intest:=true;
  randomize;
  while (intest and not TestIndices) do
    Application.Processmessages;
end;

function TForm1.TestIndices: boolean;
var
  i:integer;
  temp:string;
begin
  result:=false;
  Memo1.Clear;
  FillIndices;
  temp:='';
  for i:=0 to high(HIndices) do begin
    temp:=temp+format('%3d,',[HIndices[i]]);
    if (i mod 10)=9 then begin
      Memo1.Lines.Add(temp);
      temp:='';
    end;
  end;
  Memo1.Lines.Add(temp);

  Memo2.Clear;
  i:=FillValues;
  if i=high(WordList) then begin
    label3.Caption:='OK';
    result:=true;
  end
  else
    label3.Caption:=IntToStr(i);
  temp:='';
  for i:=0 to high(HValues) do begin
    temp:=temp+format('%3d,',[HValues[i]]);
    if (i mod 10)=9 then begin
      Memo2.Lines.Add(temp);
      temp:='';
    end;
  end;
  Memo2.Lines.Add(temp);
end;

end.
