unit ReadPalettes;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, Controls, ComCtrls, Forms,
  {$IFDEF VCL}
  Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QDialogs,
  {$ENDIF VisualCLX}
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
  PackageWizardList: TStringList;
  RegisterClassList: TStringList;
  FullRegisterClassList: TStringList;

  OldSpyRegisterPropertyEditor: TRegisterPropertyEditorProc;
  OldSpyRegisterComponentEditor: TRegisterComponentEditorProc;
  OldSpyRegisterCustomModule: TRegisterCustomModuleProc;
  OldSpyRegisterNoIcon: procedure(ComponentClasses: array of TComponentClass);
  OldSpyRegisterActions: procedure(const CategoryName: string;
    const AClasses: array of TBasicActionClass; Resource: TComponentClass);
  OldSpyRegisterPackageWizard: TWizardRegisterProc;

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
    List[I] := Format('%u;%s', [I+1, List[I]]);
end;

procedure AddPackageNames(List: TStringList);
var
  I, J, K, N: Integer;
  ModSvc: IOTAModuleServices;
  Module: IOTAModule;
  ModuleInfo: IOTAModuleInfo;
  Proj: IOTAProject;
  UnitName: string;
  Flag: Boolean;
begin
  ModSvc := (BorlandIDEServices as IOTAModuleServices);
  for I := 0 to List.Count - 1 do
  begin
    UnitName := List[I];
    N := Pos(';', UnitName);
    while N > 0 do
    begin
      UnitName := Copy(UnitName, N + 1, Length(UnitName));
      N := Pos(';', UnitName);
    end;
    UnitName := Copy(UnitName, 2, Length(UnitName) - 2);
    Flag := False;
    for J := 1 to ModSvc.ModuleCount - 1 do
    begin
      Module := ModSvc.Modules[J];
      Proj := Module as IOTAProject;
      for K := 0 to Proj.GetModuleCount - 1 do
      begin
        ModuleInfo := Proj.GetModule(K);
        if UnitName = ExtractFileName(ModuleInfo.FileName) then
        begin
          List[I] := List[I] + ';"' + ChangeFileExt(ExtractFileName(Module.FileName), '') + '"';
          Flag := True;
          Break;
        end;
      end;
      if Flag then
        Break;
    end;
  end;
end;

procedure ReadRegisterComponents;
var
  AppBuilder: TForm;
  PaletteTab: TTabControl;
  Palette: TCustomControl;
  PropInfo: PPropInfo;
  I, J: Integer;
  PalToolCount: Integer;
  OldPaletteIndex, OldToolIndex: Integer;
  SelectedToolName: string;
  LClass: TPersistentClass;
  UnitName: string;
begin
  AppBuilder := TForm(Application.FindComponent('AppBuilder'));
  PaletteTab := TTabControl(AppBuilder.FindComponent('TabControl'));
  Palette := TCustomControl(AppBuilder.FindComponent('Palette'));

  PaletteTab.Visible := False;
  OldPaletteIndex := PaletteTab.TabIndex;

  PropInfo := GetPropInfo(Palette.ClassInfo, 'SelectedIndex');
  OldToolIndex := GetOrdProp(Palette, PropInfo);

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
      LClass := GetClass(SelectedToolName);
      if (LClass = nil) or (LClass.ClassInfo = nil) then
        UnitName := ''
      else
        UnitName := GetTypeData(LClass.ClassInfo).UnitName + '.pas';
      if (Pos('Jv', PaletteTab.Tabs[I]) = 1) or (Pos('JV', PaletteTab.Tabs[I]) = 1) then
        VisibleComponentList.Add(Format('"%s";"%s";"%s"', [SelectedToolName, PaletteTab.Tabs[I], UnitName]));
    end;
  end;
  PaletteTab.TabIndex := OldPaletteIndex;
  PaletteTab.OnChange(PaletteTab);
  PropInfo := GetPropInfo(Palette.ClassInfo, 'SelectedIndex');
  SetOrdProp(Palette, PropInfo, OldToolIndex);
  PaletteTab.Visible := True;
end;

type
  TCarrier = class(TObject)
  public
    procedure GetClassCallback(AClass: TPersistentClass);
  end;

procedure TCarrier.GetClassCallback(AClass: TPersistentClass);
var
  UnitName: string;
begin
  if Pos('TJv', AClass.ClassName) = 1 then
  begin
    if AClass.ClassInfo = nil then
      UnitName := ''
    else
      UnitName := GetTypeData(AClass.ClassInfo).UnitName + '.pas';
    RegisterClassList.Add(Format('"%s";"%s"', [AClass.ClassName, UnitName]));
  end;
end;

procedure ReadRegisterClass;
var
  Carrier: TCarrier;
begin
  Carrier := TCarrier.Create;
  with TClassFinder.Create(nil, True) do
  begin
    GetClasses(Carrier.GetClassCallback);
    Free;
  end;
  Carrier.Free;
end;

procedure SpyRegisterComponentEditor(ComponentClass: TComponentClass; ComponentEditor: TComponentEditorClass);
begin
  if Assigned(ComponentClass) and Assigned(ComponentEditor) then
    if Pos('TJv', ComponentEditor.ClassName) = 1 then
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
    if Pos('TJv', EditorClass.ClassName) = 1 then
      PropertyEditorList.Add(Format('"%s";"%s";"%s";"%s"',
        [PropertyType^.Name, ComponentClass.ClassName, PropertyName, EditorClass.ClassName]));
  if Assigned(OldSpyRegisterPropertyEditor) then
    OldSpyRegisterPropertyEditor(PropertyType, ComponentClass, PropertyName, EditorClass);
end;

procedure SpyRegisterCustomModule(Group: Integer;
  ComponentBaseClass: TComponentClass; CustomModuleClass: TCustomModuleClass);
var
  UnitName: string;
begin
  if Assigned(OldSpyRegisterCustomModule) then
    OldSpyRegisterCustomModule(Group, ComponentBaseClass, CustomModuleClass);
  if Assigned(ComponentBaseClass) and Assigned(CustomModuleClass) then
  begin
    if ComponentBaseClass.ClassInfo = nil then
      UnitName := ''
    else
      UnitName := GetTypeData(ComponentBaseClass.ClassInfo).UnitName + '.pas';
    CustomModuleList.Add(Format('"%s";"%s";"%s"',
      [ComponentBaseClass.ClassName, CustomModuleClass.ClassName, UnitName]));
  end;
end;

procedure SpyRegisterNoIcon(ComponentClasses: array of TComponentClass);
var
  I: Integer;
  UnitName: string;
begin
  if Assigned(OldSpyRegisterNoIcon) then
    OldSpyRegisterNoIcon(ComponentClasses);
  for I := Low(ComponentClasses) to High(ComponentClasses) do
  begin
    if ComponentClasses[I].ClassInfo = nil then
      UnitName := ''
    else
      UnitName := GetTypeData(ComponentClasses[I].ClassInfo).UnitName + '.pas';
    NoIconList.Add(Format('"%s";"%s"', [ComponentClasses[I].ClassName, UnitName]));
  end;
end;

procedure SpyRegisterActions(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
var
  I: Integer;
  UnitName: string;
begin
  if Assigned(OldSpyRegisterActions) then
    OldSpyRegisterActions(CategoryName, AClasses, Resource);
  for I := Low(AClasses) to High(AClasses) do
  begin
    if AClasses[I].ClassInfo = nil then
      UnitName := ''
    else
      UnitName := GetTypeData(AClasses[I].ClassInfo).UnitName + '.pas';
    if Assigned(Resource) then
      ActionsList.Add(Format('"%s";"%s";"%s";"%s"', [AClasses[I].ClassName, CategoryName, Resource.ClassName, UnitName]))
    else
      ActionsList.Add(Format('"%s";"%s";"";"%s"', [AClasses[I].ClassName, CategoryName, UnitName]));
  end;
end;

procedure CleanList(DeleterList, DeletedList: TStringList);
var
  I, J: Integer;
  S: string;
begin
  for I := 0 to DeleterList.Count - 1 do
  begin
    J := Pos(';', DeleterList[I]);
    S := Copy(DeleterList[I], 1, J - 1);
    for J := 0 to DeletedList.Count - 1 do
      if Pos(S, DeletedList[J]) = 1 then
      begin
        DeletedList.Delete(J);
        Break;
      end;
  end;
end;

function SpyRegisterPackageWizard(const Wizard: IOTAWizard): Boolean;
begin
  Result := False;
  if Assigned(OldSpyRegisterPackageWizard) then
    Result := OldSpyRegisterPackageWizard(Wizard);
  if Pos('JVCL.', Wizard.GetIDString) = 1 then
    PackageWizardList.Add(Format('"%s";"%s"', [Wizard.GetName, Wizard.GetIDString]));
end;

procedure ReadReadRegisterComponentsByOTA;
var
  Pkg: IOTAPackageServices;
  I, J, N: Integer;
begin
  Pkg := (BorlandIDEServices as IOTAPackageServices);
  N := 1;
  for I := 0 to Pkg.PackageCount - 1 do
    if (Pos('jv', Pkg.PackageNames[I]) = 1) or (Pos('Jv', Pkg.PackageNames[I]) = 1) then
      for J := 0 to Pkg.ComponentCount[I] - 1 do
      begin
        AllComponentList.Add(Format('%u;"%s";"%s"', [N, Pkg.PackageNames[I], Pkg.ComponentNames[I, J]]));
        Inc(N);
      end;
  SaveFile('All JVCL components.csv', AllComponentList);
end;

procedure ReadModulesByOTA;
var
  I: Integer;
  ModSvc: IOTAModuleServices;
  List: TStringList;
begin
  List := TStringList.Create;
  ModSvc := (BorlandIDEServices as IOTAModuleServices);
  for I := 0 to ModSvc.ModuleCount - 1 do
    List.Add(ModSvc.Modules[I].FileName + ' ' + ModSvc.Modules[I].FileSystem);

  SaveFile('modules.csv', List);
  List.Free;
end;

procedure Register;
begin
  ReadRegisterComponents;
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
  PackageWizardList := TStringList.Create;
  PackageWizardList.Duplicates := dupIgnore;
  PackageWizardList.Sorted := True;
  RegisterClassList := TStringList.Create;
  RegisterClassList.Duplicates := dupIgnore;
  RegisterClassList.Sorted := True;
  FullRegisterClassList := TStringList.Create;

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
  OldSpyRegisterPackageWizard := LibraryWizardProc;
  LibraryWizardProc := SpyRegisterPackageWizard;

finalization
  RegisterPropertyEditorProc := OldSpyRegisterPropertyEditor;
  RegisterComponentEditorProc := OldSpyRegisterComponentEditor;
  RegisterCustomModuleProc := OldSpyRegisterCustomModule;
  RegisterNoIconProc := OldSpyRegisterNoIcon;
  RegisterActionsProc := OldSpyRegisterActions;
  LibraryWizardProc := OldSpyRegisterPackageWizard;

  CleanList(ActionsList, NoIconList);
  ReadRegisterClass;
  FullRegisterClassList.Assign(RegisterClassList);
  CleanList(VisibleComponentList, RegisterClassList);
  CleanList(NoIconList, RegisterClassList);
  CleanList(ActionsList, RegisterClassList);
  CleanList(CustomModuleList, RegisterClassList);

  AddIDs(VisibleComponentList);
  AddIDs(PropertyEditorList);
  AddIDs(ComponentEditorList);
  AddIDs(CustomModuleList);
  AddIDs(NoIconList);
  AddIDs(ActionsList);
  AddIDs(PackageWizardList);
  AddIDs(RegisterClassList);
  AddIDs(FullRegisterClassList);

  AddPackageNames(CustomModuleList);
  AddPackageNames(NoIconList);
  AddPackageNames(ActionsList);
  AddPackageNames(VisibleComponentList);
  AddPackageNames(RegisterClassList);
  AddPackageNames(FullRegisterClassList);

  VisibleComponentList.Insert(0, '"ID";"Component";"Palette";"FileName";"JVCLPackage"');
  PropertyEditorList.Insert(0, '"ID";"PropertyType_Name";"ComponentClass_ClassName";"PropertyName";"EditorClass_ClassName"');
  ComponentEditorList.Insert(0, '"ID";"ComponentClass_ClassName";"ComponentEditor_ClassName"');
  CustomModuleList.Insert(0, '"ID";"ComponentBaseClass_ClassName";"CustomModuleClass_ClassName";"FileName";"JVCLPackage"');
  NoIconList.Insert(0, '"ID";"ClassName";"FileName";"JVCLPackage"');
  ActionsList.Insert(0, '"ID";"ClassName";"CategoryName";"Resource_ClassName";"FileName";"JVCLPackage"');
  PackageWizardList.Insert(0, '"ID";"WizardName";"WizardIDString"');
  RegisterClassList.Insert(0, '"ID";"ClassName";"FileName";"JVCLPackage"');
  FullRegisterClassList.Insert(0, '"ID";"ClassName";"FileName";"JVCLPackage"');
  SaveFile('JVCL Visible Components.csv', VisibleComponentList);
  SaveFile('JVCL Property Editors.csv', PropertyEditorList);
  SaveFile('JVCL Component Editors.csv', ComponentEditorList);
  SaveFile('JVCL Custom Modules.csv', CustomModuleList);
  SaveFile('JVCL No Icon Components.csv', NoIconList);
  SaveFile('JVCL Actions.csv', ActionsList);
  SaveFile('JVCL Package Wizards.csv', PackageWizardList);
  SaveFile('JVCL Registered Classes.csv', RegisterClassList);
  SaveFile('JVCL All Registered Classes.csv', FullRegisterClassList);
  FreeAndNil(VisibleComponentList);
  FreeAndNil(AllComponentList);
  FreeAndNil(PropertyEditorList);
  FreeAndNil(ComponentEditorList);
  FreeAndNil(CustomModuleList);
  FreeAndNil(NoIconList);
  FreeAndNil(ActionsList);
  FreeAndNil(PackageWizardList);
  FreeAndNil(RegisterClassList);
  FreeAndNil(FullRegisterClassList);

end.
