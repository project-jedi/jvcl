unit ReadPalettes;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, Controls, ComCtrls, Forms,
  {$IFDEF WINDOWS}
  Dialogs,
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  QDialogs,
  {$ENDIF LINUX}
  TypInfo, SysUtils,
  ToolsAPI;

var
  VisibleComponentList: TStringList;
  AllComponentList: TStringList;

procedure ReadRegisterComponents;
var
  AppBuilder: TForm;
  PaletteTab: TTabControl;
  Palette: TCustomControl;
  PropInfo: PPropInfo;
  I, J, N: Integer;
  PalToolCount: Integer;
  OldPaletteIndex, OldToolIndex: Integer;
  SelectedToolName: string;
  AClass: TPersistentClass;
  UnitName: string;
begin
  AppBuilder := TForm(Application.FindComponent('AppBuilder'));
  PaletteTab := TTabControl(AppBuilder.FindComponent('TabControl'));
  Palette := TCustomControl(AppBuilder.FindComponent('Palette'));

  PaletteTab.Visible := False;
  OldPaletteIndex := PaletteTab.TabIndex;

  PropInfo := GetPropInfo(Palette.ClassInfo, 'SelectedIndex');
  OldToolIndex := GetOrdProp(Palette, PropInfo);

  N := 1;
  VisibleComponentList.Add('"ID";"Palette";"Component";"FileName"');
  for I := 0 to PaletteTab.Tabs.Count - 1 do
  begin
    PaletteTab.TabIndex := I;
    PaletteTab.OnChange(PaletteTab);
    PropInfo := GetPropInfo(Palette.ClassInfo, 'PalToolCount');
    PalToolCount := GetOrdProp(Palette, PropInfo);
    for J := 0 to PalToolCount - 1 do
    begin
      PropInfo := GetPropInfo(Palette.ClassInfo, 'SelectedIndex');
      SetOrdProp(Palette, PropInfo, J);
      PropInfo := GetPropInfo(Palette.ClassInfo, 'SelectedToolName');
      SelectedToolName := GetStrProp(Palette, PropInfo);
      AClass := GetClass(SelectedToolName);
      if (AClass = nil) or (AClass.ClassInfo = nil) then
        UnitName := 'Unable to get RTTI'
      else
        UnitName := GetTypeData(AClass.ClassInfo).UnitName;
      if (Pos('JV', PaletteTab.Tabs[I]) = 1) or (Pos('Jv', PaletteTab.Tabs[I]) = 1) then
      begin
        VisibleComponentList.Add(Format('%u;"%s";"%s";"%s.pas"', [N, PaletteTab.Tabs[I], SelectedToolName, UnitName]));
        Inc(N);
      end;
    end;
  end;
  PaletteTab.TabIndex := OldPaletteIndex;
  PaletteTab.OnChange(PaletteTab);
  PropInfo := GetPropInfo(Palette.ClassInfo, 'SelectedIndex');
  SetOrdProp(Palette, PropInfo, OldToolIndex);
  PaletteTab.Visible := True;

  with TSaveDialog.Create(nil) do
  begin
    DefaultExt := 'csv';
    Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    FileName := 'Visible JVCL components.csv';
    if Execute then
      VisibleComponentList.SaveToFile(FileName);
    Free;
  end;
end;

procedure ReadRegisterComponentEditor;
begin
end;

procedure ReadRegisterPropertyEditor;
begin
end;

procedure ReadRegisterCustomModule;
begin
end;

procedure ReadRegisterPackageWizard;
begin
end;

procedure ReadRegisterNoIcon;
begin
end;

procedure ReadRegisterActions;
begin
end;

procedure ReadRegisterClass;
begin
end;

{$IFDEF COMPILER7_UP}
procedure ReadGroupDescendentsWith;
begin
end;
{$ENDIF COMPILER7_UP}

procedure ReadReadRegisterComponentsByToolsAPI;
var
  PackageTest: IOTAPackageServices;
  I, J, N: Integer;
begin
  PackageTest := (BorlandIDEServices as IOTAPackageServices);
  N := 1;
  for I := 0 to PackageTest.PackageCount - 1 do
    if (Pos('jv', PackageTest.PackageNames[I]) = 1) or (Pos('Jv', PackageTest.PackageNames[I]) = 1) then
      for J := 0 to PackageTest.ComponentCount[I] - 1 do
      begin
        AllComponentList.Add(Format('%u;"%s";"%s"', [N, PackageTest.PackageNames[I], PackageTest.ComponentNames[I, J]]));
        Inc(N);
      end;
  with TSaveDialog.Create(nil) do
  begin
    DefaultExt := 'csv';
    Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    FileName := 'All JVCL components.csv';
    if Execute then
      AllComponentList.SaveToFile(FileName);
    Free;
  end;
end;

procedure Register;
begin
  ReadRegisterComponents;
  ReadRegisterComponentEditor;
  ReadRegisterPropertyEditor;
  ReadRegisterCustomModule;
  ReadRegisterPackageWizard;
  ReadRegisterNoIcon;
  ReadRegisterActions;
  ReadRegisterClass;
  {$IFDEF COMPILER7_UP}
  ReadGroupDescendentsWith;
  {$ENDIF COMPILER7_UP}
end;

initialization
  VisibleComponentList := TStringList.Create;
  AllComponentList := TStringList.Create;

finalization
  FreeAndNil(VisibleComponentList);
  FreeAndNil(AllComponentList);

end.
