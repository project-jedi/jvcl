unit Logging;

interface

uses
  SysUtils, Classes, Contnrs;

type
  TLog = class(TObject)
  private
    FList: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FileAdd(const Filename: string);
    procedure DirAdd(const Dir: string);
    procedure PathListAdd(const Kind, Dir: string);
    procedure PackageAdd(const Filename: string);
    procedure SaveToFile(const Filename: string);
  end;

implementation

uses
  Configuration;

{ TLog }

constructor TLog.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Add('Version:' + Config.Target.Name + ' ' + Config.Target.VersionStr);
  FList.Add('Title:' + Config.Title);
end;

destructor TLog.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TLog.FileAdd(const Filename: string);
begin
  FList.Add('FileAdd:' + Filename);
end;

procedure TLog.DirAdd(const Dir: string);
begin
  FList.Add('DirAdd:' + Dir);
end;

procedure TLog.PathListAdd(const Kind, Dir: string);
begin
  FList.Add('PathListAdd:' + Kind + ',' + Dir);
end;

procedure TLog.PackageAdd(const Filename: string);
begin
  FList.Add('PackageAdd:' + Filename);
end;

procedure TLog.SaveToFile(const Filename: string);
var
  Lines, SortedLines: TStrings;
  i: Integer;
begin
  if FileExists(Filename) then
  begin
    Lines := TStringList.Create;
    SortedLines := TStringList.Create;
    try
      Lines.LoadFromFile(Filename);
      SortedLines.Assign(Lines);
      TStringList(SortedLines).Sorted := True;
      
      for i := FList.Count - 1 downto 0 do
        if SortedLines.IndexOf(FList[i]) >= 0 then
          FList.Delete(i);
      Lines.AddStrings(FList);
      Lines.SaveToFile(Filename);
    finally
      Lines.Free;
    end;
  end
  else
    FList.SaveToFile(Filename);
end;

end.
