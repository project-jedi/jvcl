{$I JVCL.INC}

unit InfoFrm;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmInfo = class(TForm)
    reInfo: TRichEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure LoadFile(const Filename: string);
  public
    class procedure View(const Filename, Title: string);
  end;

implementation

{$R *.dfm}


procedure TfrmInfo.LoadFile(const Filename: string);
var S: TStringlist; i: integer;
begin
  if not FileExists(Filename) then
  begin
    reInfo.Lines.Text := Format('"%s": file not found', [Filename]);
    Exit;
  end;
  S := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    for i := 0 to S.Count - 1 do
      if Pos('EXPORT', S[i]) = 1 then
      begin
        S[i] := Copy(S[i], Pos(#39, S[i]) + 1, MaxInt);
        S[i] := trim(Copy(S[i], 1, Length(S[i]) - 1));
      end
      else
        S[i] := trim(S[i]);
    S.Insert(2, 'EXPORTED FUNCTIONS:'#13#10'===================');
    reInfo.Lines := S;
    reInfo.SelStart := 0;
    reInfo.Perform(EM_SCROLLCARET, 0, 0);
  finally
    S.Free;
  end;
end;

class procedure TfrmInfo.View(const Filename, Title: string);
var
  frmInfo: TfrmInfo;
  i:integer;
begin
  frmInfo := nil;
  for i := 0 to Screen.FormCount -1 do
    if Screen.Forms[i] is TfrmInfo then
    begin
      frmInfo := TfrmInfo(Screen.Forms[i]);
      Break;
    end;
  if frmInfo = nil then
    frmInfo := self.Create(Application);
  frmInfo.Caption := Format('Viewing content of %s', [Title]);
  frmInfo.LoadFile(Filename);
  frmInfo.Show;
end;

procedure TfrmInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.

