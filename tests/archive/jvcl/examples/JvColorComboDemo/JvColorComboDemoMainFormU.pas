unit JvColorComboDemoMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvColorCombo, StdCtrls, JvCombobox;

type
  TJvColorComboDemoMainForm = class(TForm)
    JvColorComboBox1: TJvColorComboBox;
    memInfo: TMemo;
    btnColorNames: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edNameTemplate: TEdit;
    chkAllowCustom: TCheckBox;
    btnCustColors: TButton;
    Label3: TLabel;
    cbDisplayStyle: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnColorNamesClick(Sender: TObject);
    procedure JvColorComboBox1NewColor(Sender: TObject; Color: TColor;
      var DisplayName: string; var AllowAdd: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCustColorsClick(Sender: TObject);
    procedure cbDisplayStyleChange(Sender: TObject);
  private
    { Private declarations }
    procedure LoadSettings;
    procedure SaveSettings;
  end;

var
  JvColorComboDemoMainForm: TJvColorComboDemoMainForm;

implementation
uses
  IniFiles;

{$R *.dfm}

procedure TJvColorComboDemoMainForm.FormCreate(Sender: TObject);
begin
  JvColorComboBox1.Options := [coText, coSysColors, coCustomColors];
  JvColorComboBox1.GetColors;
  LoadSettings;
  cbDisplayStyle.ItemIndex := 1;
  cbDisplayStyleChange(nil);
end;

procedure TJvColorComboDemoMainForm.btnColorNamesClick(Sender: TObject);
begin
  memInfo.Lines := JvColorComboBox1.ColorNameMap;
end;

procedure TJvColorComboDemoMainForm.JvColorComboBox1NewColor(Sender: TObject; Color: TColor;
  var DisplayName: string; var AllowAdd: Boolean);
begin
  // make sure the new color isn't already in the list
  AllowAdd := JvColorComboBox1.FindColor(Color) < 0;
  if AllowAdd then
  begin
    if edNameTemplate.Text <> '' then
    // CustomColorCount isn't incremented until *after* this event has finished with AllowAdd = true, so add 1 here:
      DisplayName := Format(edNameTemplate.Text, [JvColorComboBox1.CustomColorCount + 1]);
  end;
end;

procedure TJvColorComboDemoMainForm.LoadSettings;
var CurColor: TColor;
  S: string;
  SL: TStringlist;
  i: integer;
begin
  // load color name map and saved custom colors
  CurColor := JvColorComboBox1.ColorValue;
  JvColorComboBox1.Options := JvColorComboBox1.Options - [coCustomColors];
  try
    S := ChangeFileExt(Application.ExeName, '.ini');
    if FileExists(S) then
    begin
      SL := TStringlist.Create;
      try
        with TIniFile.Create(S) do
        try
          edNameTemplate.Text := ReadString('Settings', 'Custom Template', edNameTemplate.Text);
          ReadSectionValues('Color Names', JvColorComboBox1.ColorNameMap);
          ReadSection('Custom Colors', SL);
          for i := 0 to SL.Count - 1 do
            JvColorComboBox1.AddColor(StringToColor(SL[i]),'');
        finally
          Free;
        end;
      finally      
        SL.Free;
      end;
    end;
  finally
    JvColorComboBox1.ColorValue := CurColor;
    cbDisplayStyleChange(nil);
  end;
end;

procedure TJvColorComboDemoMainForm.SaveSettings;
var i: integer; AList: TList;
begin
  // save color name map and current custom colors
  JvColorComboBox1.Options := JvColorComboBox1.Options - [coCustomColors];
  try
    with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
    try
      AList := TList.Create;
      try
        JvColorComboBox1.GetCustomColors(AList);
        EraseSection('Custom Colors');
        for i := 0 to AList.Count - 1 do
          WriteString('Custom Colors', ColorToString(integer(AList[i])), '');
      finally
        AList.Free;
      end;
      WriteString('Settings', 'Custom Template', edNameTemplate.Text);
      // save color map so users can translate them if they wish
      // no need to save the custom names since they are
      // set dynamically at load time (see JvColorComboBox1NewColor)
      EraseSection('Color Names');
      with JvColorComboBox1.ColorNameMap do
        for i := 0 to Count - 1 do
          WriteString('Color Names', Names[i], Values[Names[i]]);
    finally
      Free;
    end;
  finally
    cbDisplayStyleChange(nil);
  end;
end;

procedure TJvColorComboDemoMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
end;

procedure TJvColorComboDemoMainForm.btnCustColorsClick(Sender: TObject);
var AList: TList; i: integer;
begin
  AList := TList.Create;
  memInfo.Lines.Clear;
  try
    // the returned TList contains a list of TColor items
    JvColorComboBox1.GetCustomColors(AList);
    for i := 0 to AList.Count - 1 do
      memInfo.Lines.Add(ColorToString(integer(AList[i])));
  finally
    AList.Free;
  end;
end;

procedure TJvColorComboDemoMainForm.cbDisplayStyleChange(Sender: TObject);
var O: TJvColorComboOptions;
begin
  O := JvColorComboBox1.Options;
  O := O - [coText, coHex, coRGB, coCustomColors];
  if chkAllowCustom.Checked then
    Include(O, coCustomColors);
  case cbDisplayStyle.ItemIndex of
    1:
      Include(O, coText);
    2:
      Include(O, coHex);
    3:
      Include(O, coRGB);
  end;
  JvColorComboBox1.Options := O;
end;

end.

