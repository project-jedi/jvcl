{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit UserHelp;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, JvPlacemnt, StdCtrls, JvxCtrls, Grids, Jvgrids, Mask,
  JvToolEdit;

type
  TCustomizeHelpDlg = class(TForm)
    Label1: TLabel;
    FormStorage: TJvFormStorage ;
    FileName: TJvFilenameEdit ;
    Label2: TLabel;
    HelpList: TJvDrawGrid ;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpListGetEditText(Sender: TObject; ACol, ARow: Longint;
      var Value: string);
    procedure HelpListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure HelpListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure HelpListSetEditText(Sender: TObject; ACol, ARow: Longint;
      const Value: String);
    procedure FileNameChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HelpListGetEditLimit(Sender: TObject;
      var MaxLength: Integer);
    procedure HelpListRowMoved(Sender: TObject; FromIndex,
      ToIndex: Longint);
  private
    { Private declarations }
    FHelpList: TStrings;
    FEditChanged: Boolean;
    function ApplyEditChanges: Boolean;
  protected
{$IFNDEF WIN32}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF}
  public
    { Public declarations }
  end;

procedure CustomizeHelp(UserHelpList: TStrings);

implementation

uses JvVCLUtils;

{$R *.DFM}

procedure CustomizeHelp(UserHelpList: TStrings);
var
  CanSelect: Boolean;
begin
  with TCustomizeHelpDlg.Create(Application) do
  try
    FHelpList.Assign(UserHelpList);
    HelpList.RowCount := FHelpList.Count + 1;
    HelpList.Row := 0;
    HelpListSelectCell(HelpList, 1, 0, CanSelect);
    if ShowModal = mrOk then UserHelpList.Assign(FHelpList);
  finally
    Free;
  end;
end;

{ TCustomizeHelpDlg }

{$IFNDEF WIN32}
procedure TCustomizeHelpDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;
{$ENDIF}

function TCustomizeHelpDlg.ApplyEditChanges: Boolean;
var
  Res: TModalResult;
begin
  Result := not FEditChanged or (FEditChanged and (FileName.FileName = ''));
  if not Result then begin
    Res := MessageDlg('Apply changes to current item?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if Res = mrYes then begin
      if HelpList.Row < FHelpList.Count then begin
        FHelpList[HelpList.Row] := GetShortHint(FHelpList[HelpList.Row]) +
          '|' + FileName.FileName;
        Result := True;
      end;
    end
    else if Res = mrCancel then begin
      SysUtils.Abort;
    end;
  end;
  FEditChanged := False;
end;

procedure TCustomizeHelpDlg.FormCreate(Sender: TObject);
begin
  FHelpList := TStringList.Create;
  HelpList.ClientHeight := (HelpList.DefaultRowHeight+ 1) * 5 - 1;
end;

procedure TCustomizeHelpDlg.FormDestroy(Sender: TObject);
begin
  FHelpList.Free;
end;

procedure TCustomizeHelpDlg.FileNameChange(Sender: TObject);
begin
  FEditChanged := True;
end;

procedure TCustomizeHelpDlg.OkBtnClick(Sender: TObject);
begin
  ApplyEditChanges;
end;

procedure TCustomizeHelpDlg.HelpListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  ApplyEditChanges;
  if Row < FHelpList.Count then FileName.FileName := GetLongHint(FHelpList[Row])
  else FileName.FileName := '';
  FEditChanged := False;
end;

procedure TCustomizeHelpDlg.HelpListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  S: string;
begin
  if (Col = 0) and (Row < FHelpList.Count) then
    HelpList.DrawStr(Rect, IntToStr(Row + 1) + ' ', taRightJustify)
  else if Col = 1 then begin
    if Row < FHelpList.Count then S := GetShortHint(FHelpList[Row])
    else S := '';
    HelpList.DrawStr(Rect, S, taLeftJustify);
  end;
end;

procedure TCustomizeHelpDlg.HelpListSetEditText(Sender: TObject; ACol,
  ARow: Longint; const Value: String);
begin
  if ARow >= FHelpList.Count then begin
    if (Value <> '') then
      FHelpList.Add(Value + '|' + FileName.FileName)
    else begin
      Beep;
      Abort;
    end;
  end
  else begin
    if (Value = '') then FHelpList.Delete(ARow)
    else FHelpList[ARow] := Value + '|' + GetLongHint(FHelpList[ARow]);
  end;
  HelpList.RowCount := FHelpList.Count + 1;
end;

procedure TCustomizeHelpDlg.HelpListGetEditText(Sender: TObject; ACol,
  ARow: Longint; var Value: string);
begin
  if ARow < FHelpList.Count then Value := GetShortHint(FHelpList[ARow])
  else Value := '';
end;

procedure TCustomizeHelpDlg.HelpListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CanSelect: Boolean;
begin
  if (Key = VK_DELETE) and ((Shift = []) or (ssCtrl in Shift)) then begin
    if (HelpList.Row < FHelpList.Count) and not HelpList.EditorMode then begin
      if MessageDlg('Delete selected item?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        FHelpList.Delete(HelpList.Row);
        HelpList.RowCount := FHelpList.Count + 1;
        HelpListSelectCell(HelpList, HelpList.Col, HelpList.Row, CanSelect);
      end;
    end;
  end
  else if (Shift = []) and (HelpList.Row = FHelpList.Count - 1) and
    (Key = VK_DOWN) then
  begin
    HelpList.RowCount := FHelpList.Count + 1;
  end;
end;

procedure TCustomizeHelpDlg.HelpListGetEditLimit(Sender: TObject;
  var MaxLength: Integer);
begin
  MaxLength := 50;
end;

procedure TCustomizeHelpDlg.HelpListRowMoved(Sender: TObject; FromIndex,
  ToIndex: Longint);
begin
  if (ToIndex >= 0) and (ToIndex < FHelpList.Count) and
    (FromIndex >= 0) and (FromIndex < FHelpList.Count) then
  begin
    FHelpList.Move(FromIndex, ToIndex);
    HelpList.Invalidate;
  end;
end;

end.
