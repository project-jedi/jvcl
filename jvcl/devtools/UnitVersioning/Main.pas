unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan, Mask, JvExMask, JvToolEdit, ComCtrls,
  JvComponent, JvSearchFiles, dpp_PascalParser;

type
  TFormMain = class(TForm)
    BtnExecute: TButton;
    BtnQuit: TButton;
    ProgressBar: TProgressBar;
    DEditDir: TJvDirectoryEdit;
    XPManifest1: TXPManifest;
    JvSearchFiles: TJvSearchFiles;
    CheckBoxSubDirs: TCheckBox;
    EditLogPath: TEdit;
    Label1: TLabel;
    procedure BtnQuitClick(Sender: TObject);
    procedure BtnExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure ProcessFile(const Filename: string);
    function ParseUses(Parser: TPascalParser): Boolean;
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

uses
  StrUtils;

{$R *.dfm}

var
  ConditionStack: TStrings;
  IsUseJVCL: Boolean;

procedure TFormMain.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.BtnExecuteClick(Sender: TObject);
var
  Dir: string;
  i: Integer;
begin
  Dir := DEditDir.Text;
  BtnExecute.Enabled := False;
  try
    JvSearchFiles.RootDirectory := Dir;
    if CheckBoxSubDirs.Checked then
      JvSearchFiles.DirOption := doIncludeSubDirs
    else
      JvSearchFiles.DirOption := doExcludeSubDirs;

    if JvSearchFiles.Search then
    begin
      ProgressBar.Position := 0;
      ProgressBar.Max := JvSearchFiles.Files.Count;
      for i := 0 to JvSearchFiles.Files.Count - 1 do
      begin
        if Pos('\cvs\', AnsiLowerCase(JvSearchFiles.Files[i])) = 0 then
          ProcessFile(JvSearchFiles.Files[i]);
        ProgressBar.StepIt;
        Application.ProcessMessages;
      end;
    end;
  finally
    BtnExecute.Enabled := True;
  end;
end;

function NextToken(Parser: TPascalParser; out Token: PTokenInfo): Boolean;
begin
  Result := True;
  while Parser.GetToken(Token) do
  begin
    if Token.Kind <> tkComment then
      Exit
    else
    begin
      if Token.ExKind = tekOption then
      begin
        if AnsiStartsText('{$ENDIF', Token.Value) or AnsiStartsText('{$IFEND', Token.Value) then
        begin
          ConditionStack.Delete(ConditionStack.Count - 1);
        end
        else
        if AnsiStartsText('{$IF', Token.Value) then
        begin
          ConditionStack.Add(Token.Value);
          if Pos('USEJVCL', AnsiUpperCase(Token.Value)) > 0 then
            IsUseJVCL := True;
        end;
      end;
    end;
  end;
  Result := False;
end;

// returns True if the "JclUnitVersioning" unit is in the uses list
function TFormMain.ParseUses(Parser: TPascalParser): Boolean;
var
  Token: PTokenInfo;
begin
  Result := False;

  while NextToken(Parser, Token) do
  begin
    case Token.Kind of
      tkSymbol:
        if Token.Value = ';' then
          Break; // uses-end
      tkIdent:
        if CompareText(Token.Value, 'in') = 0 then
        begin
          if CompareText(Token.Value, 'JclUnitVersioning') = 0 then
          begin
            Result := True;
            Exit;
          end;
        end;
      //tkString: ;
    end;
  end;
end;

procedure TFormMain.ProcessFile(const Filename: string);
var
  s: string;
  Stream: TFileStream;
  Token: PTokenInfo;
  UsesToken, ImplToken, InitToken, FiniToken, LastEndToken: TTokenInfo;
  Parser: TPascalParser;
  InImplementation: Boolean;
  Modified, HasImplUses, HasInit, HasFini: Boolean;
  sl: TStrings;
  ConstDecl: string;
begin
  Modified := False;
  IsUseJVCL := False;
  Stream := TFileStream.Create(Filename, fmOpenReadWrite or fmShareExclusive);
  ConditionStack := TStringList.Create;
  try
    SetLength(s, Stream.Size);
    Stream.Read(s[1], Length(s));

    Parser := TPascalParser.Create('', s);
    try
      s := ''; // release memory
      FillChar(ImplToken, 0, SizeOf(ImplToken));
      FillChar(LastEndToken, 0, SizeOf(LastEndToken));

      HasInit := False;
      HasFini := False;
      HasImplUses := False;
      InImplementation := False;
      while NextToken(Parser, Token) do
      begin
        if Token.Kind = tkIdent then
        begin
          if CompareText(Token.Value, 'implementation') = 0 then
          begin
            ImplToken := Token^;
            InImplementation := True;
          end
          else if InImplementation then
          begin
            if CompareText(Token.Value, 'uses') = 0 then
            begin
              HasImplUses := True;
              UsesToken := Token^; // save
              if not ParseUses(Parser) then
              begin
                Parser.Insert(UsesToken.EndIndex + 1,
                              sLineBreak +
                              '  {$IFDEF UNITVERSIONING}' + sLineBreak +
                              '  JclUnitVersioning,' + sLineBreak +
                              '  {$ENDIF UNITVERSIONING}');
                Parser.Index := UsesToken.EndIndex + 1;
                Modified := True;
              end;
            end
            else if CompareText(Token.Value, 'initialization') = 0 then
            begin
              HasInit := True;
              InitToken := Token^;
              if ConditionStack.Count > 0 then
                ShowMessage('initialization is IFDEFed: ' + Filename);
            end
            else if CompareText(Token.Value, 'finalization') = 0 then
            begin
              HasFini := True;
              FiniToken := Token^;
              if ConditionStack.Count > 0 then
                ShowMessage('finalization is IFDEFed: ' + Filename);
            end
            else if CompareText(Token.Value, 'end') = 0 then
               LastEndToken := Token^;
          end;
        end;
      end;

      if not HasImplUses and InImplementation then
      begin
        Modified := True;
        // do not change the text here because this would offset the index of
        // InitToken and LastEndToken
      end;

      if Modified then
      begin
        ConstDecl :=
          'const' + sLineBreak +
          '  UnitVersioning: TUnitVersionInfo = (' + sLineBreak +
          '    RCSfile: ''$' + 'RCSfile$'';' + sLineBreak +
          '    Revision: ''$' + 'Revision$'';' + sLineBreak +
          '    Date: ''$' + 'Date$'';' + sLineBreak +
          '    LogPath: ''' + Trim(EditLogPath.Text) + '''' + sLineBreak +
          '  );' + sLineBreak;

        if not HasInit then
        begin
          if LastEndToken.StartIndex = 0 then
            raise Exception.CreateFmt('Invalid .pas file: %s', [Filename]);
          Parser.Insert(LastEndToken.StartIndex,
                        '{$IFDEF UNITVERSIONING}' + sLineBreak +
                        ConstDecl +
                        sLineBreak +
                        'initialization' + sLineBreak +
                        '  RegisterUnitVersion(HInstance, UnitVersioning);' + sLineBreak +
                        sLineBreak +
                        'finalization' + sLineBreak +
                        '  UnregisterUnitVersion(HInstance);' + sLineBreak +
                        '{$ENDIF UNITVERSIONING}' + sLineBreak +
                        sLineBreak);
        end
        else
        begin
          if not HasFini then
          begin
            if LastEndToken.StartIndex = 0 then
              raise Exception.CreateFmt('Invalid .pas file: %s', [Filename]);
            Parser.Insert(LastEndToken.StartIndex,
                          sLineBreak +
                          '{$IFDEF UNITVERSIONING}' + sLineBreak +
                          'finalization' + sLineBreak +
                          '  UnregisterUnitVersion(HInstance);' + sLineBreak +
                          '{$ENDIF UNITVERSIONING}' + sLineBreak +
                          sLineBreak);
          end
          else
          begin
           Parser.Insert(FiniToken.EndIndex + 1,
                        sLineBreak +
                        '  {$IFDEF UNITVERSIONING}' + sLineBreak +
                        '  UnregisterUnitVersion(HInstance);' + sLineBreak +
                        '  {$ENDIF UNITVERSIONING}');
          end;

          Parser.Insert(InitToken.EndIndex + 1,
                        sLineBreak +
                        '  {$IFDEF UNITVERSIONING}' + sLineBreak +
                        '  RegisterUnitVersion(HInstance, UnitVersioning);' + sLineBreak +
                        '  {$ENDIF UNITVERSIONING}' + sLineBreak);

          Parser.Insert(InitToken.StartIndex,
                        '{$IFDEF UNITVERSIONING}' + sLineBreak +
                        ConstDecl +
                        '{$ENDIF UNITVERSIONING}' + sLineBreak +
                        sLineBreak);
        end;
      end;

      if not HasImplUses and InImplementation then
      begin
        Parser.Insert(ImplToken.EndIndex + 1,
                      sLineBreak +
                      sLineBreak +
                      '{$IFDEF UNITVERSIONING}' + sLineBreak +
                      'uses' + sLineBreak +
                      '  JclUnitVersioning;' + sLineBreak +
                      '{$ENDIF UNITVERSIONING}');
        if IsUseJVCL then
        begin
          Parser.Insert(ImplToken.StartIndex,
                        '{$IFNDEF USEJVCL}' + sLineBreak +
                        '  {$UNDEF UNITVERSIONING}' + sLineBreak +
                        '{$ENDIF ~USEJVCL}' + sLineBreak +
                        sLineBreak);
        end;
      end;

      if Modified then
      begin
        sl := TStringList.Create;
        sl.Text := Parser.Text;
        sl.SaveToFile(ExtractFilePath(Filename) + '\new\' + ExtractFileName(Filename));
        sl.Free;
        {Stream.Position := 0;
        Stream.Size := 0; // truncate
        Stream.Write(Parser.Text[1], Length(Parser.Text));}
      end;

    finally
      Parser.Free;
    end;
  finally
    FreeAndNil(ConditionStack);
    Stream.Free;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  DEditDir.Text := ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))) + PathDelim + 'run';
end;

end.
