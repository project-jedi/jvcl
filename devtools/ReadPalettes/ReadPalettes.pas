unit ReadPalettes;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, Controls, ComCtrls, Forms,
  {$IFDEF MSWINDOWS}
  Dialogs,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QDialogs,
  {$ENDIF UNIX}
  TypInfo, SysUtils, ActnList,
  DesignIntf, ToolsAPI;

var
  VisibleComponentList: TStringList;
  AllComponentList: TStringList;
  PropertyEditorList: TStringList;
  ComponentEditorList: TStringList;
  CustomModuleList: TStringList;
  NoIconList: TStringList;
  ActionsList: TStringList;

  OldSpyRegisterPropertyEditor: TRegisterPropertyEditorProc;
  OldSpyRegisterComponentEditor: TRegisterComponentEditorProc;
  OldSpyRegisterCustomModule: TRegisterCustomModuleProc;
  OldSpyRegisterNoIcon: procedure(ComponentClasses: array of TComponentClass);
  OldSpyRegisterActions: procedure (const CategoryName: string;
    const AClasses: array of TBasicActionClass; Resource: TComponentClass) = nil;

procedure SaveFile(AFileName: string; List: TStringList);
begin
  with TSaveDialog.Create(nil) do
  begin
    DefaultExt := 'csv';
    Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    FileName := AFileName;
    if Execute then
      List.SaveToFile(FileName);
    Free;
  end;
end;

procedure AddIDs(List: TStringList);
var
  I: Integer;
begin
  List.Sorted := False;
  for I := 0 to List.Count - 1 do
    List[I] := Format('%u;%s', [I, List[I]]);
end;

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
end;

procedure SpyRegisterComponentEditor(ComponentClass: TComponentClass; ComponentEditor: TComponentEditorClass);
begin
  if Assigned(ComponentClass) and Assigned(ComponentEditor) then
    if (Pos('TJv', ComponentClass.ClassName) = 1) or (Pos('TJv', ComponentEditor.ClassName) = 1) then
      ComponentEditorList.Add(Format('"%s";"%s"',
        [ComponentClass.ClassName, ComponentEditor.ClassName]));
  if Assigned(OldSpyRegisterComponentEditor) then
    OldSpyRegisterComponentEditor(ComponentClass, ComponentEditor);
end;

procedure SpyRegisterPropertyEditor(PropertyType: PTypeInfo;
  ComponentClass: TClass; const PropertyName: string;
  EditorClass: TPropertyEditorClass);
begin
  if Assigned(PropertyType) and Assigned(ComponentClass) and Assigned(EditorClass) then
    if (Pos('TJv', PropertyType^.Name) = 1) or (Pos('TJv', ComponentClass.ClassName) = 1) or (Pos('TJv', EditorClass.ClassName) = 1) then
      PropertyEditorList.Add(Format('"%s";"%s";"%s";"%s"',
        [PropertyType^.Name, ComponentClass.ClassName, PropertyName, EditorClass.ClassName]));
  if Assigned(OldSpyRegisterPropertyEditor) then
    OldSpyRegisterPropertyEditor(PropertyType, ComponentClass, PropertyName, EditorClass);
end;

procedure SpyRegisterCustomModule(Group: Integer;
  ComponentBaseClass: TComponentClass; CustomModuleClass: TCustomModuleClass);
begin
  if Assigned(ComponentBaseClass) and Assigned(CustomModuleClass) then
    //if (Pos('TJv', ComponentBaseClass.ClassName) = 1) or (Pos('TJv', CustomModuleClass.ClassName) = 1) then
      CustomModuleList.Add(Format('%u;"%s";"%s"',
        [Group, ComponentBaseClass.ClassName, CustomModuleClass.ClassName]));
  if Assigned(OldSpyRegisterCustomModule) then
    OldSpyRegisterCustomModule(Group, ComponentBaseClass, CustomModuleClass);
end;

procedure SpyRegisterNoIcon(ComponentClasses: array of TComponentClass);
var
  I: Integer;
begin
  for I := Low(ComponentClasses) to High(ComponentClasses) do
    NoIconList.Add(Format('"%s"', [ComponentClasses[I].ClassName]));
  if Assigned(OldSpyRegisterNoIcon) then
    OldSpyRegisterNoIcon(ComponentClasses);
end;

procedure SpyRegisterActions(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    if Assigned(Resource) then
      ActionsList.Add(Format('"%s";"%s";"%s"', [CategoryName, AClasses[I].ClassName, Resource.ClassName]))
    else
      ActionsList.Add(Format('"%s";"%s";""', [CategoryName, AClasses[I].ClassName]));
  if Assigned(OldSpyRegisterActions) then
    OldSpyRegisterActions(CategoryName, AClasses, Resource);
end;

procedure CleanNoIconList;
var
  I, N: Integer;
  S: string;
begin
  for I := 0 to ActionsList.Count - 1 do
  begin
    N := Pos(';', ActionsList[I]);
    S := Copy(ActionsList[I], N + 1, Length(ActionsList[I]));
    N := Pos(';', S);
    S := Copy(S, 1, N - 1);
    if NoIconList.Find(S, N) then
      NoIconList.Delete(N);
  end;
end;

procedure ReadRegisterPackageWizard;
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
  SaveFile('All JVCL components.csv', AllComponentList);
end;

procedure Register;
begin
  ReadRegisterComponents;
  ReadRegisterPackageWizard;
  ReadRegisterClass;
  {$IFDEF COMPILER7_UP}
  ReadGroupDescendentsWith;
  {$ENDIF COMPILER7_UP}
end;

initialization
  VisibleComponentList := TStringList.Create;
  AllComponentList := TStringList.Create;
  PropertyEditorList := TStringList.Create;
  PropertyEditorList.Duplicates := dupIgnore;
  PropertyEditorList.Sorted := True;
  ComponentEditorList := TStringList.Create;
  ComponentEditorList.Duplicates := dupIgnore;
  ComponentEditorList.Sorted := True;
  CustomModuleList := TStringList.Create;
  CustomModuleList.Duplicates := dupIgnore;
  CustomModuleList.Sorted := True;
  NoIconList := TStringList.Create;
  NoIconList.Duplicates := dupIgnore;
  NoIconList.Sorted := True;
  ActionsList := TStringList.Create;
  ActionsList.Duplicates := dupIgnore;
  ActionsList.Sorted := True;

  OldSpyRegisterPropertyEditor := RegisterPropertyEditorProc;
  RegisterPropertyEditorProc := SpyRegisterPropertyEditor;
  OldSpyRegisterComponentEditor := RegisterComponentEditorProc;
  RegisterComponentEditorProc := SpyRegisterComponentEditor;
  OldSpyRegisterCustomModule := RegisterCustomModuleProc;
  RegisterCustomModuleProc := SpyRegisterCustomModule;
  OldSpyRegisterNoIcon := RegisterNoIconProc;
  RegisterNoIconProc := SpyRegisterNoIcon;
  OldSpyRegisterActions := RegisterActionsProc;
  RegisterActionsProc := SpyRegisterActions;

finalization
  RegisterPropertyEditorProc := OldSpyRegisterPropertyEditor;
  RegisterComponentEditorProc := OldSpyRegisterComponentEditor;
  RegisterCustomModuleProc := OldSpyRegisterCustomModule;
  RegisterNoIconProc := OldSpyRegisterNoIcon;
  RegisterActionsProc := OldSpyRegisterActions;

  CleanNoIconList;

  AddIDs(PropertyEditorList);
  AddIDs(ComponentEditorList);
  AddIDs(CustomModuleList);
  AddIDs(NoIconList);
  AddIDs(ActionsList);

  PropertyEditorList.Insert(0, '"ID";"PropertyType_Name";"ComponentClass_ClassName";"PropertyName";"EditorClass_ClassName"');
  ComponentEditorList.Insert(0, '"ID";"ComponentClass_ClassName";"ComponentEditor_ClassName"');
  CustomModuleList.Insert(0, '"ID";"Group";"ComponentBaseClass_ClassName";"CustomModuleClass_ClassName"');
  NoIconList.Insert(0, '"ID";"ClassName"');
  ActionsList.Insert(0, '"ID";"CategoryName";"ClassName";"Resource_ClassName"');
  SaveFile('JVCL Visible Components.csv', VisibleComponentList);
  SaveFile('JVCL Property Editors.csv', PropertyEditorList);
  SaveFile('JVCL Component Editors.csv', ComponentEditorList);
  SaveFile('JVCL Custom Modules.csv', CustomModuleList);
  SaveFile('JVCL No Icon Components.csv', NoIconList);
  SaveFile('JVCL Actions.csv', ActionsList);
  FreeAndNil(VisibleComponentList);
  FreeAndNil(AllComponentList);
  FreeAndNil(PropertyEditorList);
  FreeAndNil(ComponentEditorList);
  FreeAndNil(CustomModuleList);
  FreeAndNil(NoIconList);
  FreeAndNil(ActionsList);

end.
