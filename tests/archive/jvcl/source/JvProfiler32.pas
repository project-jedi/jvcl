{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProfiler32.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Certified Software Corp. [certsoft@quest-net.com]
Portions created by Peter Thörnqvist are Copyright (C) 1996 Certified Software Corp.
All Rights Reserved.

Contributor(s): Peter Thörnqvist [peter3@peter3.com]

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ @abstract(Simple profiler) }
unit JvProfiler32;
{ To Do:
 Use QueryPerformanceCounter / Frequency instead of GetTickCount (the high resolution timer)
}

interface
uses
  Windows, Dialogs, ComCtrls, StdCtrls, Controls, Classes, ExtCtrls, Forms,
    JvComponent;

const
  MaxProfEntries = 1024;                { maximum number of "blocks" to profile }
  MaxStackSize = 1024; { maximum nesting of blocks at any one time }

var
  OddClick: boolean = true;

type
  TJvProfileInfo = array[0..MaxProfEntries - 1] of record
    InOutTime: LongInt;
    TimeSpent: LongInt;
    Calls: LongInt;
    StringID: string;
  end;

  TProcStack = array[1..MaxStackSize] of record
    CallerID: integer;
    EntryTime: LongInt;
  end;

  TProfReport = class(TForm)
    Panel1: TPanel;
    SaveBtn: TButton;
    lvReport: TListView;
    Panel2: TPanel;
    OKBtn: TButton;
    TrimBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure lvReportColumnClick(Sender: TObject; Column: TListColumn);
    procedure SaveBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure TrimBtnClick(Sender: TObject);
  public
    StartTime: integer;
    EndTime: integer;
    LastProc: integer;
    ProfileInfo: TJvProfileInfo;
  end;

  TJvProfiler = class(TJvComponent)
  private
    FProfileInfo: TJvProfileInfo;
    FNames: TStrings;
    FStack: TProcStack;
    FStartTime: LongInt;
    FEndTime: LongInt;
    FLastProc: integer;
    FStackSize: integer;
    FEnabled: boolean;
    FStarted: boolean;
    FSorted: boolean;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure SetNames(Value: TStrings);
    procedure SetEnabled(Value: boolean);
    procedure SetSorted(Value: boolean);
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure Initialize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure EnterID(ID: integer);
    procedure EnterName(const Name: string);
    procedure ExitName(const Name: string);
    procedure ExitID(ID: integer);
    procedure Stop;
    procedure ShowReport;
  published
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property Names: TStrings read FNames write SetNames;
    property Sorted: boolean read FSorted write SetSorted default false;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation
uses
  SysUtils, JvTypes;

const
  DefCaption = 'Profiler 32 Report';
  EmptyLine = '0.00';
  DefHeader = 'Profiler 32 run %s by "%s" (machine %s).';
  DefHeader2 =
    'Profiler 32 - (C) 1996 Certified Software Corp, portions Copyright (C) 1997 by Peter Thörnqvist; all rights reserved.';

{$R *.DFM}

type
  PProfType = ^TProfType;
  TProfType = record
    InOutTime,
      TimeSpent,
      Calls: integer;
    StringID: string;
  end;

  PStackType = ^TStackType;
  TStackType = record
    CallerID,
      EntryTime: integer;
  end;

function GetUserNamePas: string;
var Buff: array[0..255] of char;
  i: Cardinal;
begin
  i := 255;
  GetUserName(Buff, i);
  Result := Buff;
end;

function GetComputerNamePas: string;
var Buff: array[0..255] of char;
  i: Cardinal;
begin
  i := 255;
  GetComputerName(Buff, i);
  Result := Buff;
end;

{ TJvProfiler }

constructor TJvProfiler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNames := TStringList.Create;
end;

procedure TJvProfiler.Initialize;
var i: integer;
begin
  FEnabled := false;
  FStarted := false;
  FStartTime := 0;
  FEndTime := 0;
  FStackSize := 0;
  FLastProc := -1;
  { build ID list }
  for i := 0 to FNames.Count - 1 do
  begin
    if Length(Trim(FNames[i])) < 1 then
      Continue;                         { skip empty ID's }
    if FLastProc > MaxProfEntries then
      raise EJVCLException.CreateFmt('Max number of ID''s exceeded (%d)',
        [MaxProfEntries - 1]);
    Inc(FLastProc);
    with FProfileInfo[FLastProc] do
    begin
      TimeSpent := 0;
      Calls := 0;
      StringID := FNames[i];
    end;
  end;

end;

destructor TJvProfiler.Destroy;
begin
  Stop;
  FNames.Free;
  inherited Destroy;
end;

procedure TJvProfiler.EnterID(ID: integer);
var
  Snap: integer;
begin
  if FEnabled then
  begin
    Snap := GetTickCount;
    if FStackSize > MaxStackSize then
      raise EJVCLException.CreateFmt('Max stack size exceeded (%d)',
        [MaxStackSize]);
    Inc(FStackSize);

    with FStack[FStackSize] do
    begin
      EntryTime := Snap;
      CallerID := ID
    end;

    with FProfileInfo[ID] do
    begin
      Inc(Calls);
      InOutTime := Snap;
    end;
  end;
end;

procedure TJvProfiler.EnterName(const Name: string);
begin
  EnterID(TStringLIst(FNames).IndexOf(Name));
end;

procedure TJvProfiler.ExitName(const Name: string);
begin
  ExitID(TStringList(FNames).IndexOf(Name));
end;

procedure TJvProfiler.ExitID(ID: integer);
var
  Snap, Elapsed: integer;
begin
  if Enabled then
  begin
    Snap := GetTickCount;
    with FProfileInfo[ID] do
    begin
      Elapsed := Snap - InOutTime;
      TimeSpent := TimeSpent + Elapsed;
    end;
    if FStackSize > 0 then
      Dec(FStackSize);
    if FStackSize > 0 then
      with FProfileInfo[FStack[FStackSize].CallerID] do
        TimeSpent := TimeSpent - Elapsed;
  end;
end;

procedure TJvProfiler.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(self);
end;

procedure TJvProfiler.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(self);
end;

procedure TJvProfiler.Start;
begin
  if FEnabled and not FStarted then
  begin
    //    Initialize;
    DoStart;
    FStartTime := GetTickCount;
    FStarted := true;
  end;
end;

procedure TJvProfiler.Stop;
begin
  if FEnabled and FStarted then
  begin
    FEndTime := GetTickCount;
    DoStop;
    FStarted := false;
  end;
end;

procedure TJvProfiler.SetNames(Value: TStrings);
begin
  FNames.Assign(Value);
  Initialize;
end;

procedure TJvProfiler.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
end;

procedure TJvProfiler.SetSorted(Value: boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    TStringList(FNames).Sorted := FSorted;
    Initialize;
  end;
end;

procedure TJvProfiler.ShowReport;
begin
  if FEnabled then
  begin
    if FStarted then
      Stop;
    with TProfReport.Create(nil) do
    begin
      EndTime := FEndTime;
      StartTime := FStartTime;
      LastProc := FLastProc;
      ProfileInfo := FProfileInfo;
      ShowModal;
      Free;
    end;
  end;
end;

procedure TProfReport.FormShow(Sender: TObject);
var
  ThisProc: integer;
  TotalSum: integer;
  LItem: TListItem;
begin
  OddClick := true;
  TotalSum := (EndTime - StartTime);
  if TotalSum = 0 then
    Exit;
  lvReport.Items.BeginUpdate;
  lvReport.Items.Clear;
  for ThisProc := 0 to LastProc do
    with ProfileInfo[ThisProc] do
    begin
      LItem := lvReport.Items.Add;
      LItem.Caption := StringID;        { function ID }
      if Calls <> 0 then
      begin
        LItem.SubItems.Add(Format('%4.2f', [TimeSpent * 1.0]));  { Total time spent here }
        LItem.Subitems.Add(IntToStr(Calls)); { Total number of calls }
        LItem.SubItems.Add(Format('%4.2f', [TimeSpent / Calls]));  { average time }
        LItem.SubItems.Add(Format('%4.2f', [TimeSpent / TotalSum * 100]));  { percentage }
      end
      else
      begin
        LItem.SubItems.Add(EmptyLine);
        LItem.SubItems.Add('0');
        LItem.SubItems.Add(EmptyLine);
        LItem.SubItems.Add(EmptyLine);
      end;
    end;
  Caption := Format('%s -  total elapsed time: %d (ms)', [DefCaption,
    TotalSum]);
  lvReport.Items.EndUpdate;
end;

function IsFloat(S: string): boolean;
begin
  Result := true;
  try
    StrToFloat(S);
  except
    Result := false;
  end;
end;

function DefSort(lParam1, lParam2: TListItem; lParamSort: integer): integer
  stdcall;
var l1, l2: Extended;
begin
  if lParamSort = 0 then
    Result := AnsiCompareText(lParam1.Caption, lParam2.Caption)
  else
  begin
    if not IsFloat(lParam1.SubItems[lParamSort - 1]) then
      l1 := -1
    else
      l1 := StrToFloat(lParam1.SubItems[lParamSort - 1]);
    if not IsFloat(lParam2.SubItems[lParamSort - 1]) then
      l2 := -1
    else
      l2 := StrToFloat(lParam2.SubItems[lParamSort - 1]);
    Result := round((l1 * 1000) - (l2 * 1000));
  end;
  if OddClick then
    Result := -Result;
end;

procedure TProfReport.lvReportColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  //  lvReport.Items.BeginUpdate;
  lvReport.CustomSort(@DefSort, Column.Index);
  OddClick := not OddClick;
  //  lvReport.Items.EndUpdate;
end;

procedure TProfReport.SaveBtnClick(Sender: TObject);
var OutList: TStringList;
  S: string;
  i, j: integer;
begin
  with TSaveDialog.Create(nil) do
  begin
    Filter := 'Text formats|*.asc;*.txt;*.inf;*.doc|All files|*.*';
    if Execute then
    begin
      OutList := TStringList.Create;
      OutList.Add(Format(DefHeader, [DateToStr(Now), GetUserNamePas,
        GetComputerNamePas]));
      OutList.Add(DefHeader2);
      S := '';
      for i := 0 to lvReport.Columns.Count - 1 do
        S := S + lvReport.Columns[i].Caption + #9;
      OutList.Add(S);
      S := '';
      with lvReport do
        for i := 0 to Items.Count - 1 do
        begin
          with Items[i] do
          begin
            S := S + Caption + #9;
            for j := 0 to SubItems.Count - 1 do
              S := S + SubItems[j] + #9;
            OutList.Add(S);
          end;
          S := '';
        end;
      OutList.SaveToFile(Filename);
      OutList.Free;
    end;
    Free;
  end;
end;

procedure TProfReport.OKBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TProfreport.TrimBtnClick(Sender: TObject);
var i: integer;
begin
  lvReport.Items.BeginUpdate;
  for i := lvReport.Items.Count - 1 downto 0 do
    { no calls = not used }
    if AnsiCompareText(lvReport.Items[i].SubItems[1], '0') = 0 then
      lvReport.Items.Delete(i);
  lvReport.Items.EndUpdate;
end;

end.

