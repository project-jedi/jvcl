unit DUnit_TestSpell;

interface

uses
  CLasses,
 TestFrameWork, jvSpellIntf;

type
  TTestSpell = class(TTestCase)
  private
    fcSpellChecker: IJvSpellChecker;
    fcUserDict: TStringList;

    procedure DoCreateSpellChecker;

    function GetMisspelledWords(const ps: string): string;

  protected
    procedure Setup; override;
    procedure Teardown; override;
  published

    procedure MinimalTest;

    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
  end;


implementation

uses
  SysUtils,
  JclStrings,
  JvSpellChecker;

procedure TTestSpell.Setup;
begin
  inherited;

  fcUserDict := TStringList.Create;

  DoCreateSpellChecker;
end;

procedure TTestSpell.Teardown;
begin
  fcSpellChecker := nil;
  FreeAndNil(fcUserDict);

  inherited;
end;


procedure TTestSpell.DoCreateSpellChecker;
const
    { you'll have to change these }
  DICT_PATH = 'F:\Program\Borland\Add\JEDI\jvcl\Dict\English.dic';
  USER_DICT_PATH = 'F:\Program\Borland\Add\JEDI\jvcl\Dict\user.dic';
begin
  if fcSpellChecker = nil then
  begin
    Assert(Assigned(CreateSpellChecker));
    fcSpellChecker := CreateSpellChecker;

    Assert(FileExists(DICT_PATH));
    Assert(FileExists(USER_DICT_PATH));

    fcSpellChecker.Dictionary := DICT_PATH;

    fcUserDict.LoadFromFile(USER_DICT_PATH);

    fcSpellChecker.UserDictionary := fcUserDict;
  end;

end;

function TTestSpell.GetMisspelledWords(const ps: string): string;
var
  liStartIndex, liWordLength: integer;
  lbMore: boolean;
  lsWord: string;
begin
  fcSpellChecker.Text := ps;
  fcSpellChecker.Seek(1);

  liStartIndex := 0;
  liWordLength := 0;
  Result := '';

  try
    repeat
      lbMore := fcSpellChecker.Next(liStartIndex, liWordLength);
      if not lbMore then
        break;

      if (liStartIndex >= 0) and (liWordLength > 0 ) then
      begin
        lsWord := Copy(ps, liStartIndex, liWordLength);

        { don't add a word more than once }
        if StrFind(lsWord, Result) < 1 then
        begin
          if Result <> '' then
            Result := Result + ' ';
          Result := Result + lsWord;
        end;

      end;

    until False;
  except
    on E: Exception do
    begin
      Result := 'Error ' + E.Message;
    end;
  end;
end;

procedure TTestSpell.MinimalTest;
begin
  GetMisspelledWords('');
  GetMisspelledWords(' ');
  GetMisspelledWords('  ');
  
  GetMisspelledWords('a');
  GetMisspelledWords('aa');
  GetMisspelledWords('aaa');
  GetMisspelledWords('aaa');
  GetMisspelledWords('aaaa');
  GetMisspelledWords('aaaaa');
end;

procedure TTestSpell.Test1;
var
  lsOut: string;
begin
  { 'then' is in the dictionary, 'slurm' and 'glagnar' aren't
    Space on the end  }
  lsOut := GetMisspelledWords('slurm then glagnar ');

  Check(Pos('slurm', lsOut) > 0);
  Check(Pos('glagnar', lsOut) > 0);
  Check(Pos('then', lsOut) = 0);
end;

procedure TTestSpell.Test2;
var
  lsOut: string;
begin
  { no space }
  lsOut := GetMisspelledWords('slurm then glagnar');

  Check(Pos('slurm', lsOut) > 0);
  Check(Pos('glagnar', lsOut) > 0);
  Check(Pos('then', lsOut) = 0);
end;

procedure TTestSpell.Test3;
var
  lsOut: string;
begin
  lsOut := GetMisspelledWords('glagnar then slurm ');

  Check(Pos('slurm', lsOut) > 0);
  Check(Pos('glagnar', lsOut) > 0);
  Check(Pos('then', lsOut) = 0);
end;

procedure TTestSpell.Test4;
var
  lsOut: string;
begin
  lsOut := GetMisspelledWords('glagnar then slurm');

  Check(Pos('slurm', lsOut) > 0);
  Check(Pos('glagnar', lsOut) > 0);
  Check(Pos('then', lsOut) = 0);
end;

initialization
 TestFramework.RegisterTest(TTestSpell.Suite);
end.
case FTP:
			if ( file_exists( $v_diskfile ) ) {
				readfile( $v_diskfile );
			} else {
				$ftp = file_ftp_connect();
				file_ftp_get ( $ftp, $v_diskfile, $v_filename );
				file_ftp_disconnect( $ftp );
				readfile( $v_diskfile );
			}
			break;
		default:
			echo $v_content;
	}
?>