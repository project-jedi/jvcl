unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  Math, StdCtrls, JvHidControllerClass;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    WriteButton: TButton;
    ReportID: TEdit;
    Label1: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    ReadButton: TButton;
    InfoButton: TButton;
    HidCtl: TJvHidDeviceController;
    GetFeatureButton: TButton;
    SetFeatureButton: TButton;
    ListBox2: TListBox;
    procedure HidCtlDeviceChange(Sender: TObject);
    function HidCtlEnumerate(const HidDev: TJvHidDevice;
      const Index: Integer): Boolean;
    procedure WriteButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReadButtonClick(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
  public
    DevList: TList;
    Edits: array[0..63] of TEdit;
    procedure DoRead(Feature: Boolean);
    procedure DoWrite(Feature: Boolean);
  end;

var
  Form1: TForm1;
  TheDev: TJvHidDevice;

implementation

uses Unit2;

{$R *.DFM}

procedure TForm1.HidCtlDeviceChange(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(ListBox1) then
  begin
    if DevList <> nil then
    begin
      for I := 0 to DevList.Count-1 do
      begin
        TheDev := DevList.Items[I];
        TheDev.Free;
      end;
      DevList.Clear;
    end
    else
      DevList := TList.Create;
    ListBox1.Clear;
    HidCtl.Enumerate;
    if ListBox1.Items.Count > 0 then
    begin
      ListBox1.ItemIndex := 0;
      ListBox1Click(Self);
    end;
  end;
end;

function TForm1.HidCtlEnumerate(const HidDev: TJvHidDevice;
  const Index: Integer): Boolean;
var
  Dev: TJvHidDevice;
begin
  if Assigned(ListBox1) then
  begin
    if HidDev.ProductName <> '' then
      ListBox1.Items.Add(HidDev.ProductName)
    else
      ListBox1.Items.Add(Format('Device VID=%x PID=%x',
        [HidDev.Attributes.VendorID,
         HidDev.Attributes.ProductID]));
    HidCtl.CheckOutByIndex(Dev, Index);
    DevList.Add(Dev);
  end;
  Result := True;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  I, J: Integer;
begin
  if Edits[0] <> nil then
    Exit;
  Edits[0] := Edit1;
  for I := 1 to 63 do
    Edits[I] := TEdit.Create(Self);
  for J := 0 to 3 do
    for I := 0 to 15 do
      with Edits[J*16 + I] do
      begin
        Visible  := False;
        Left     := Edit1.Left + I*(Edit1.Width+2);
        Top      := Edit1.Top  + J*(Edit1.Height+2);
        Width    := Edit1.Width;
        Anchors  := Edit1.Anchors;
        Parent   := Edit1.Parent;
        TabOrder := 2 + J*16 + I;
      end;
  HidCtlDeviceChange(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DevList.Count-1 do
  begin
    TheDev := DevList.Items[I];
    HidCtl.CheckIn(TheDev);
  end;
  DevList.Free;
end;

procedure TForm1.InfoButtonClick(Sender: TObject);
begin
  if (ListBox1.Items.Count > 0) and
     (ListBox1.ItemIndex >= 0) then
    with TInfoForm.Create(Self) do
    begin
      ShowModal;
      Free;
    end;
end;

procedure TForm1.DoRead(Feature: Boolean);
var
  I: Integer;
  Buf: array [0..64] of Byte;
  ToRead: Cardinal;
  Read: Cardinal;
  Str: string;
begin
  if (ListBox1.Items.Count > 0) and
     (ListBox1.ItemIndex >= 0) then
  begin
    if Feature then
      ToRead := TheDev.Caps.FeatureReportByteLength
    else
      ToRead := TheDev.Caps.InputReportByteLength;
    FillChar(Buf, SizeOf(Buf), 0);
    Buf[0] := StrToIntDef('$' + ReportID.Text, 0);
    Label3.Caption :=
      Format('Reading %d Bytes...', [ToRead]);
    for I := 0 to 63 do
      Edits[I].Text := '';
    Update;
    Read := 0;
    if Feature then
    begin
      if TheDev.GetFeature(Buf, ToRead) then
        Read := ToRead;
    end
    else
      TheDev.ReadFile(Buf, ToRead, Read);
    Label3.Caption := Format('%d Bytes read', [Read]);
    ReportID.Text := Format('%.2x', [Buf[0]]);
    for I := 1 to Read-1 do
      Edits[I-1].Text := Format('%.2x', [Buf[I]]);
    if Feature then
      Str := 'F '
    else
      Str := 'R ';
    Str := Str + Format('%.2x  ', [Buf[0]]);
    for I := 1 to Read-1 do
      Str := Str + Format('%.2x ', [Buf[I]]);
    ListBox2.Items.Add(Str);
  end;
end;

procedure TForm1.DoWrite(Feature: Boolean);
var
  I: Integer;
  Buf: array [0..64] of Byte;
  Written: Cardinal;
  ToWrite: Cardinal;
begin
  Buf[0] := StrToIntDef('$' + ReportID.Text, 0);
  ReportID.Text := Format('%.2x', [Buf[0]]);
  ToWrite := 0;
  for I := 63 downto 0 do
    if Edits[I].Text <> '' then
    begin
      ToWrite := I+1;
      Break;
    end;
  for I := 1 to ToWrite do
  begin
    Buf[I] := StrToIntDef('$' + Edits[I-1].Text, 0);
    Edits[I-1].Text := Format('%.2x', [Buf[I]]);
  end;
  Inc(ToWrite);

  if (ListBox1.Items.Count > 0) and
     (ListBox1.ItemIndex >= 0) and
     (ToWrite > 1) then
  begin
    TheDev := DevList.Items[ListBox1.ItemIndex];
    Label3.Caption :=
      Format('Writing %d Bytes...', [ToWrite]);
    Update;
    Written := 0;
    if Feature then
    begin
      if TheDev.SetFeature(Buf, ToWrite) then
        Written := ToWrite;
    end
    else
      TheDev.WriteFile(Buf, ToWrite, Written);
    Label3.Caption :=
      Format('%d Bytes written', [Written]);
  end;
end;

procedure TForm1.ReadButtonClick(Sender: TObject);
begin
  DoRead(Sender = GetFeatureButton);
end;

procedure TForm1.WriteButtonClick(Sender: TObject);
begin
  DoWrite(Sender = SetFeatureButton);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  I: Integer;
  N: Word;
begin
  TheDev := DevList.Items[ListBox1.ItemIndex];
  ListBox2.Clear;
  if Assigned(Edits[1]) then
  begin
    N := TheDev.Caps.InputReportByteLength;
    N := Max(N, TheDev.Caps.OutputReportByteLength);
    N := Max(N, TheDev.Caps.FeatureReportByteLength);
    for I := 0 to N-1 do
      Edits[I].Visible := True;
    for I := N-1 to 63 do
      Edits[I].Visible := False;
  end;
end;

end.
