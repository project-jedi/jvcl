{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{                                                       }
{*******************************************************}

unit SqlMon;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, JvSplit, DB,
  ComCtrls, Grids, Menus, JvComponent, JvFormPlacement, JvGrids,
  JvExExtCtrls, JvExGrids;

type
  TTraceSQL = class(TForm)
    Splitter: TJvxSplitter ;
    ViewPanel: TMemo;
    FormStorage: TJvFormStorage ;
    TraceBox: TJvDrawGrid ;
    SaveLogDialog: TSaveDialog;
    PopupMenu: TPopupMenu;
    miPopupCopy: TMenuItem;
    miPopupClear: TMenuItem;
    miPopupSelectAll: TMenuItem;
    miSaveLog: TMenuItem;
    N1: TMenuItem;
    procedure TraceBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TraceBoxDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure miPopupSelectAllClick(Sender: TObject);
    procedure miPopupClearClick(Sender: TObject);
    procedure miPopupCopyClick(Sender: TObject);
    procedure miSaveLogClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateData;
  public
    { Public declarations }
    procedure Clear;
  end;

var
  TraceSQL: TTraceSQL;

procedure BufAddLine(const Msg: string);
procedure BufClear(Confirm: Boolean);
procedure BufSetSize(Value: Integer);

implementation

uses
  Options, JvJCLUtils, Math;

{$R *.DFM}

var
  TraceBuffer: TStrings = nil;
  BufSize: Integer = 256;
  CurBufSize: Longint = 0;
  TraceForm: TTraceSQL = nil;

procedure CheckEmpty;
begin
  if TraceBuffer.Count = 0 then
    raise Exception.Create('SQL log buffer is empty');
end;

procedure CheckBufferSize(AddSize: Integer);
var
  ItemSize: Integer;
begin
  while (CurBufSize + AddSize) div 1024 >= BufSize do begin
    ItemSize := Length(TraceBuffer[0]) + SizeOf(Longint) + 1;
    TraceBuffer.Delete(0);
    Dec(CurBufSize, ItemSize);
  end;
end;

procedure BufAddLine(const Msg: string);
begin
  CheckBufferSize(Length(Msg) + SizeOf(Longint) + 1);
  TraceBuffer.AddObject(Msg, TObject(DateTimeToFileDate(SysUtils.Now)));
  if TraceForm <> nil then TraceForm.UpdateData;
end;

procedure BufClear(Confirm: Boolean);
begin
  CheckEmpty;
  if Confirm then begin
    case MessageDlg('Ok to clear log buffer?', mtConfirmation,
      mbYesNoCancel, 0) of
      mrYes: TraceBuffer.Clear;
      mrCancel: SysUtils.Abort;
    end;
  end
  else TraceBuffer.Clear;
  if TraceForm <> nil then TraceForm.UpdateData;
end;

procedure BufSetSize(Value: Integer);
begin
  if (BufSize > Value) and (TraceBuffer.Count > 0) then BufClear(True);
  BufSize := Max(Value, MinSQLTraceBuffer);
end;

{ TTraceSQL }

procedure TTraceSQL.Clear;
begin
  ViewPanel.Lines.Clear;
  BufClear(True);
  UpdateData;
end;

procedure TTraceSQL.UpdateData;
begin
  TraceBox.RowCount := Max(2, TraceBuffer.Count + 1);
  TraceBox.Row := Max(1, TraceBuffer.Count);
  TraceBoxClick(nil);
end;

procedure TTraceSQL.TraceBoxClick(Sender: TObject);
begin
  ViewPanel.Lines.Clear;
  if (TraceBox.Row > 0) and (TraceBox.Row <= TraceBuffer.Count) then begin
    ViewPanel.Lines.Add(TraceBuffer[TraceBox.Row - 1]);
  end;
end;

procedure TTraceSQL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TTraceSQL.FormShow(Sender: TObject);
begin
  UpdateData;
  TraceForm := Self;
  ActiveControl := TraceBox;
  FormResize(Self);
end;

procedure TTraceSQL.FormDestroy(Sender: TObject);
begin
  TraceForm := nil;
end;

procedure TTraceSQL.FormResize(Sender: TObject);
begin
  TraceBox.ColWidths[2] := ClientWidth - (TraceBox.ColWidths[0] +
    TraceBox.ColWidths[1]);
end;

procedure TTraceSQL.TraceBoxDrawCell(Sender: TObject; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState);
var
  S: string;
  Align: TAlignment;
begin
  Align := taLeftJustify;
  S := '';
  if Row = 0 then begin
    case Col of
      0: S := 'No.';
      1: S := 'Time Stamp';
      2: S := 'SQL Statement';
    end;
  end
  else if Row <= TraceBuffer.Count then begin
    case Col of
      0: begin
           S := IntToStr(Row) + ' ';
           Align := taRightJustify;
         end;
      1: begin
           S := FormatDateTime('hh:mm:ss', FileDateToDateTime(
             Longint(TraceBuffer.Objects[Row - 1])));
           Align := taCenter;
         end;
      2: S := TraceBuffer[Row - 1];
    end;
  end;
  TraceBox.DrawStr(Rect, S, Align);
end;

procedure TTraceSQL.miPopupSelectAllClick(Sender: TObject);
begin
  ViewPanel.SelectAll;
  ActiveControl := ViewPanel;
end;

procedure TTraceSQL.miPopupClearClick(Sender: TObject);
begin
  Self.Clear;
end;

procedure TTraceSQL.miPopupCopyClick(Sender: TObject);
begin
  ViewPanel.CopyToClipboard;
end;

procedure TTraceSQL.miSaveLogClick(Sender: TObject);
var
  S: string;
  I: Integer;
  Stream: TStream;
begin
  CheckEmpty;
  if SaveLogDialog.Execute then begin
    Stream := TFileStream.Create(SaveLogDialog.FileName, fmCreate);
    try
      for I := 0 to TraceBuffer.Count - 1 do begin
        S := FormatDateTime('hh:mm:ss  ', FileDateToDateTime(
          Longint(TraceBuffer.Objects[I]))) + TraceBuffer[I] + #10#13;
        Stream.WriteBuffer(Pointer(S)^, Length(S));
      end;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TTraceSQL.PopupMenuPopup(Sender: TObject);
var
  NotEmpty: Boolean;
begin
  miPopupCopy.Enabled := ViewPanel.SelLength > 0;
  NotEmpty := (TraceBuffer.Count > 0);
  miPopupClear.Enabled := NotEmpty;
  miPopupSelectAll.Enabled := NotEmpty;
  miSaveLog.Enabled := NotEmpty;
end;

initialization
  TraceBuffer := TStringList.Create;
finalization
  TraceBuffer.Free;
end.
