unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Generator, ComCtrls, JclFileUtils, JclDateTime;

type
  TFormMain = class(TForm)
    BtnProcess: TButton;
    ProgressBar: TProgressBar;
    LblText: TLabel;
    EditInDir: TEdit;
    EditOutDir: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ProcessListBox: TListBox;
    procedure BtnProcessClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

uses StrUtils;

{$R *.dfm}

type
  TProgressProc = procedure(const Text: string; Percentage: Integer);

procedure ProcessFiles(const Dir, OutDir: string; Progress: TProgressProc);
var
  Gen: TGenerator;
  Files: TStrings;
  i: Integer;
  sr: TSearchRec;

  function OutPath(const filename: ansistring): ansistring;
  begin
    Result := filename;
    Insert('Q', Result, 3);
    Result := outdir + '\' + Result;
  end;

  function NeedsUpdate(const filename: string): boolean;
  var
    DestName : string;
    ftSrc, ftDst : TDateTime;
  begin
    DestName := OutPath(filename);
    if FileExists(DestName) then
    begin
      ftSrc := FileTimeToDateTime(GetFileLastWrite(dir + '\' + filename));
      ftDst := FileTimeToDateTime(GetFileLastWrite(DestName));
      Result := ftSrc > ftDst
    end
    else
      Result := true;
  end;

  procedure GetFilesources(extension: string);
  var
    DestName: string;
  begin
    if FindFirst(Dir + '\' + extension, faAnyFile and not faDirectory, sr) = 0 then
    try
      repeat
        if AnsiStartsText('Jv', sr.Name) and (IsNotQFile(sr.Name)) then
        begin
          DestName := OutPath(sr.Name);
          if NeedsUpdate(sr.Name) then
            Files.Add(sr.Name);
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;


begin
  Files := TStringList.Create;
  try
    if Assigned(Progress) then
      Progress('', 0);
    ForceDirectories(OutDir);
    GetFilesources('*.pas');
    Gen := TGenerator.Create;
    try
      Gen.OutDir := OutDir;
      for i := 0 to Files.Count - 1 do
      begin
        Gen.GenerateQUnit(Dir + PathDelim + Files[i]);
        if Assigned(Progress) then
          Progress(Files[i], (i + 1) * 100 div Files.Count);
      end;
    finally
      Gen.Free;
    end;
    Files.Clear;
    FormMain.ProgressBar.Position :=  0;
    if Assigned(Progress) then
      Progress('', 0);
    GetFilesources('*.xfm');
    for i := 0 to Files.Count - 1 do
    begin
      CopyFile(PAnsiChar(Dir + PathDelim + Files[i]),
               PAnsiChar(OutPath(Files[i])), false);
      if Assigned(Progress) then
        Progress(Files[i], (i + 1) * 100 div Files.Count);
    end;
  finally
    Files.Free;
  end;
end;

procedure Progress(const Text: string; Percentage: Integer);
begin
  FormMain.LblText.Caption := Text;
  FormMain.ProgressBar.Position := Percentage;
  if Text <> '' then
    FormMain.ProcessListBox.Items.Add(Text);
  Application.ProcessMessages;
end;

procedure TFormMain.BtnProcessClick(Sender: TObject);
begin
  BtnProcess.Enabled := False;
  ProcessListBox.Clear;
  ProcessFiles(EditInDir.Text, EditOutDir.Text, Progress);
  BtnProcess.Enabled := True;
end;

end.
