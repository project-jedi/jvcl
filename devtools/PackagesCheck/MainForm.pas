{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MainForm.pas, released on 2006-02-20.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2006 Florent Ouchet.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit MainForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  JclSimpleXml, TargetInfo, JvComponentBase, JvDualList;

type
  TPackageCheckForm = class(TForm)
    ButtonTargetDefines: TButton;
    StatusBar: TStatusBar;
    ComboBoxModel: TComboBox;
    LabelModel: TLabel;
    MemoMessages: TMemo;
    JvDualListDialogSelect: TJvDualListDialog;
    ButtonGo: TButton;
    CheckBoxCLX: TCheckBox;
    CheckBoxHaltOnError: TCheckBox;
    procedure ButtonTargetDefinesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGoClick(Sender: TObject);
  private
    FXMLFileName: string;
    FTargetsInfo: TTargetsInfo;
    FSettings: TJclSimpleXml;
    FPGSettings: TJclSimpleXml;
  public

  end;

var
  PackageCheckForm: TPackageCheckForm;

implementation

{$R *.dfm}

uses
  DefineForm, UsesParser,
  JclFileUtils, JclStrings,
  PackageInformation;

procedure TPackageCheckForm.ButtonGoClick(Sender: TObject);
var
  PackageList, TargetList, AliasList: TStringList;
  PackagePath: string;

  function IsIncluded(TargetName: string; Tokens: string): Boolean;
  var
    IndexToken, IndexAlias: Integer;
    TokenStrings: TStringList;
  begin
    if Tokens = 'all' then
    begin
      Result := True;
      Exit;
    end;
    TokenStrings := TStringList.Create;
    try
      TokenStrings.CaseSensitive := False;
      TokenStrings.QuoteChar := '"';
      TokenStrings.Delimiter := ',';
      TokenStrings.DelimitedText := Tokens;

      for IndexToken := 0 to TokenStrings.Count - 1 do
      begin
        IndexAlias := AliasList.IndexOfName(TokenStrings.Strings[IndexToken]);
        if IndexAlias >= 0 then
          TokenStrings.Strings[IndexToken] := Copy(AliasList[IndexAlias], Length(TokenStrings.Strings[IndexToken]) + 2, MaxInt);
      end;

      Tokens := TokenStrings.DelimitedText;
      Tokens := StrRemoveChars(Tokens, ['"']);
      TokenStrings.DelimitedText := Tokens;

      for IndexToken := 0 to TokenStrings.Count - 1 do
        if TargetList.IndexOf(TokenStrings.Strings[IndexToken]) = -1 then
          MemoMessages.Lines.Add(Format('Unknown targetname %s', [TokenStrings.Strings[IndexToken]]));

      Result := TokenStrings.IndexOf(TargetName) >= 0;
    finally
      TokenStrings.Free;
    end;
  end;

  procedure BuildRequireList(const PackageInfo: TPackageXmlInfo;
    const TargetName: string; RequiredList: TStringList);
  var
    IndexRequire: Integer;
    ARequiredPackage: TRequiredPackage;
    IndexJVCLPackage: Integer;
  begin
    for IndexRequire := 0 to PackageInfo.RequireCount - 1 do
    begin
      ARequiredPackage := PackageInfo.Requires[IndexRequire];
      if IsIncluded(TargetName, ARequiredPackage.Targets.CommaText) then
      begin
        if RequiredList.IndexOf(ARequiredPackage.Name) = -1 then
          RequiredList.Add(ARequiredPackage.Name);
        IndexJVCLPackage := PackageList.IndexOf(ARequiredPackage.Name);
        if IndexJVCLPackage >= 0 then
          BuildRequireList(TPackageXmlInfo(PackageList.Objects[IndexJVCLPackage]),
            TargetName, RequiredList);
      end;
    end;
  end;

  procedure CheckPackageTarget(const PackageInfo: TPackageXmlInfo;
    const TargetName: string);
  var
    IncludeDirs, DependencyList, KnownUnits, PackageUsesList, DefinedSymbols: TStringList;
    IndexTarget, IndexRequire, IndexContained, IndexUnit, IndexJVCLPackage,
    IndexTargetPackage, IndexInclude, NbContained: Integer;
    ARequiredPackage: TRequiredPackage;
    AContainedFile: TContainedFile;
    ATargetInfo: TTargetInfo;
    BPackageInfo: TPackageXmlInfo;
    ATargetPackage: TTargetPackage;
    AUsesParser: TUsesParser;
    UnitFileName{, RequiredPackageName}: string;
    RequiredPackageUsed: Boolean;
  begin
    StatusBar.SimpleText := Format('Checking package %s for target %s', [PackageInfo.Name, TargetName]);

    IndexTarget := FTargetsInfo.IndexOf(TargetName);
    if IndexTarget >= 0 then
      ATargetInfo := FTargetsInfo.Infos[IndexTarget]
    else
      ATargetInfo := nil;

    KnownUnits := TStringList.Create;
    KnownUnits.CaseSensitive := False;
    IncludeDirs := TStringList.Create;
    IncludeDirs.CaseSensitive := False;
    PackageUsesList := TStringList.Create;
    PackageUsesList.CaseSensitive := False;
    DefinedSymbols := TStringList.Create;
    DefinedSymbols.CaseSensitive := False;
    DependencyList := TStringList.Create;
    DependencyList.CaseSensitive := False;

    try
      // build list of required packages for dependency
      for IndexRequire := 0 to PackageInfo.RequireCount - 1 do
      begin
        ARequiredPackage := PackageInfo.Requires[IndexRequire];
        IndexJVCLPackage := PackageList.IndexOf(ARequiredPackage.Name);
        if IsIncluded(TargetName, ARequiredPackage.Targets.CommaText)
          and (IndexJVCLPackage >= 0) then
          BuildRequireList(TPackageXmlInfo(PackageList.Objects[IndexJVCLPackage]),
            TargetName, DependencyList);
      end;

      // check that all required packages are part of the current package
      {for IndexContained := 0 to DependencyList.Count - 1 do
      begin
        RequiredPackageName := DependencyList.Strings[IndexContained];
        RequiredPackageUsed := False;
        for IndexRequire := 0 to PackageInfo.RequireCount - 1 do
        begin
          ARequiredPackage := PackageInfo.Requires[IndexRequire];
          if SameText(RequiredPackageName, ARequiredPackage.Name)
            and IsIncluded(TargetName, ARequiredPackage.Targets.CommaText) then
          begin
            RequiredPackageUsed := True;
            Break;
          end;
        end;
        if not RequiredPackageUsed then
          MemoMessages.Lines.Add(Format('Package %s need package %s for dependency', [PackageInfo.Name, RequiredPackageName]));
      end;}

      // build list of unit contained in required packages
      for IndexRequire := 0 to PackageInfo.RequireCount - 1 do
      begin
        ARequiredPackage := PackageInfo.Requires[IndexRequire];
        if IsIncluded(TargetName, ARequiredPackage.Targets.CommaText) then
        begin
          IndexJVCLPackage := PackageList.IndexOf(ARequiredPackage.Name);
          if IndexJVCLPackage >= 0 then
          begin
            // package is found in the jvcl
            BPackageInfo := TPackageXmlInfo(PackageList.Objects[IndexJVCLPackage]);
            for IndexUnit := 0 to BPackageInfo.ContainCount - 1 do
            begin
              AContainedFile := BPackageInfo.Contains[IndexUnit];
              if SameText(ExtractFileExt(AContainedFile.Name), '.pas')
                and IsIncluded(TargetName, AContainedFile.Targets.CommaText) then
                KnownUnits.AddObject(PathExtractFileNameNoExt(AContainedFile.Name), TObject(IndexRequire));
            end;
          end
          else if Assigned(ATargetInfo) then
          begin
            IndexTargetPackage := ATargetInfo.IndexOf(ARequiredPackage.Name);
            if IndexTargetPackage >= 0 then
            begin
              // package is found in the dependencies (rtl, vcl, jcl...)
              ATargetPackage := ATargetInfo.Packages[IndexTargetPackage];
              for IndexUnit := 0 to ATargetPackage.UnitCount - 1 do
                KnownUnits.AddObject(ATargetPackage.Units[IndexUnit], TObject(IndexRequire));
            end
            else
              MemoMessages.Lines.Add(Format('Processing package %s for target %s, unable to find required package %s',
                [PackageInfo.Name, TargetName, ARequiredPackage.Name]));
          end
          else
            MemoMessages.Lines.Add(Format('Processing package %s for target %s, unable to find required package %s',
              [PackageInfo.Name, TargetName, ARequiredPackage.Name]));
        end;
      end;
      // add unit contained in this package
      for IndexUnit := 0 to PackageInfo.ContainCount - 1 do
      begin
        AContainedFile := PackageInfo.Contains[IndexUnit];
        if SameText(ExtractFileExt(AContainedFile.Name), '.pas')
          and IsIncluded(TargetName, AContainedFile.Targets.CommaText) then
            KnownUnits.AddObject(PathExtractFileNameNoExt(AContainedFile.Name), TObject(-1));
      end;
  
      DefinedSymbols.Clear;
      IncludeDirs.Clear;
      if Assigned(ATargetInfo) then
      begin
        DefinedSymbols.Assign(ATargetInfo.Defines);
  
        for IndexInclude := 0 to ATargetInfo.IncludeDirs.Count - 1 do
          if PathIsAbsolute(ATargetInfo.IncludeDirs.Strings[IndexInclude]) then
            IncludeDirs.Add(ATargetInfo.IncludeDirs.Strings[IndexInclude])
          else
            IncludeDirs.Add(PathCanonicalize(PackagePath + TargetName + '\' + ATargetInfo.IncludeDirs.Strings[IndexInclude]));
      end;
  
      // parse all contained units and check for unsatisfied uses
      AUsesParser := TUsesParser.Create;
      try
        NbContained := 0;
        for IndexContained := 0 to PackageInfo.ContainCount - 1 do
        begin
          AContainedFile := PackageInfo.Contains[IndexContained];
          if IsIncluded(TargetName, AContainedFile.Targets.CommaText) then
          begin
            UnitFileName := AContainedFile.Name;
  
            StatusBar.SimpleText := Format('Parsing unit %s of package %s for target %s',
              [PathExtractFileNameNoExt(UnitFileName), PackageInfo.Name, TargetName]);
  
            if SameText(ExtractFileExt(UnitFileName), '.pas') then
            begin
              Inc(NbContained);
              UnitFileName := Format('%s%s\%s', [PackagePath, TargetName, UnitFileName]);
              UnitFileName := PathCanonicalize(UnitFileName);
              
              if AUsesParser.LoadFromFile(UnitFileName) then
              begin
                AUsesParser.Defines := DefinedSymbols;
                AUsesParser.IncludeDirs := IncludeDirs;
                AUsesParser.UsesList.Clear;
  
                if AUsesParser.ParseUses then
                begin
                  for IndexUnit := 0 to AUsesParser.UsesList.Count - 1 do
                    if KnownUnits.IndexOf(AUsesParser.UsesList.Strings[IndexUnit]) < 0 then
                      MemoMessages.Lines.Add(Format('Parsing unit %s: unable to find unit %s of package %s for target %s', [PathExtractFileNameNoExt(UnitFileName), AUsesParser.UsesList.Strings[IndexUnit], PackageInfo.Name, TargetName]));
                  PackageUsesList.AddStrings(AUsesParser.UsesList);
                end
                else
                  MemoMessages.Lines.Add(Format('Unable to parse unit %s of package %s for target %s', [PathExtractFileNameNoExt(UnitFileName), PackageInfo.Name, TargetName]));
              end
              else
                MemoMessages.Lines.Add(Format('Processing package %s for target %s, unable to locate file %s', [PackageInfo.Name, TargetName, UnitFileName]));
            end;
          end;
        end;
      finally
        AUsesParser.Free;
      end;
  
      // find required packages that were not used and not needed for dependency
      if NbContained > 0 then
        for IndexRequire := 0 to PackageInfo.RequireCount - 1 do
      begin
        ARequiredPackage := PackageInfo.Requires[IndexRequire];
        if (DependencyList.IndexOf(ARequiredPackage.Name) = -1)
          and IsIncluded(TargetName, ARequiredPackage.Targets.CommaText) then
        begin
          RequiredPackageUsed := False;
          for IndexUnit := 0 to KnownUnits.Count - 1 do
            if Integer(KnownUnits.Objects[IndexUnit]) = IndexRequire then
          begin
            RequiredPackageUsed := RequiredPackageUsed or (PackageUsesList.IndexOf(KnownUnits.Strings[IndexUnit]) >= 0);
            if RequiredPackageUsed then
              Break;
          end;
          if not RequiredPackageUsed then
            MemoMessages.Lines.Add(Format('Package %s on target %s doesn''t use any unit of package %s', [PackageInfo.Name,TargetName,PackageInfo.Requires[IndexRequire].Name]));
        end;
      end;
    finally
      KnownUnits.Free;
      IncludeDirs.Free;
      PackageUsesList.Free;
      DefinedSymbols.Free;
      DependencyList.Free;
    end;
  end;

  procedure CheckPackage(const PackageInfo: TPackageXmlInfo);
  var
    IndexTarget: Integer;
    TargetName: string;
    IsClx: Boolean;
  begin
    if (MemoMessages.Lines.Count <> 0) and CheckBoxHaltOnError.Checked then
      Exit;

    for IndexTarget := 0 to TargetList.Count - 1 do
    begin
      TargetName := TargetList.Strings[IndexTarget];
      IsClx := TargetList.Objects[IndexTarget] <> nil;
      if (not IsClx) or CheckBoxCLX.Checked then
        CheckPackageTarget(PackageInfo, TargetName);
    end;
  end;

var
  IndexPackage, IndexTarget, IndexAlias, IndexList: Integer;
  ModelsNode, ModelNode, TargetsNode, TargetNode, AliasesNode, AliasNode,
  GUINode, SelectedPackageNode: TJclSimpleXMLElem;
  APackageXmlInfo: TPackageXmlInfo;
  PackageName: string;
  IsClx: Boolean;
  PersonalProperty, IsClxProperty: TJclSimpleXMLProp;
begin
  if ComboBoxModel.ItemIndex < 0 then
    Exit;

  MemoMessages.Clear;

  ModelsNode := FPGSettings.Root.Items.ItemNamed['MODELS'];
  ModelNode := ModelsNode.Items.Item[ComboboxModel.ItemIndex];
  PackagePath := PathAddSeparator(ExtractFilePath(Application.ExeName))
    + ModelNode.Properties.ItemNamed['PACKAGES'].Value;
  PackagePath := PathAddSeparator(PathCanonicalize(PackagePath));

  GUINode := FSettings.Root.Items.ItemNamed['GUI'];
  SelectedPackageNode := GUINode.Items.ItemNamed['SELECTED'];

  PackageList := TStringList.Create;
  PackageList.CaseSensitive := False;
  TargetList := TStringList.Create;
  TargetList.CaseSensitive := False;
  AliasList := TStringList.Create;
  AliasList.CaseSensitive := False;
  try
    TargetsNode := ModelNode.Items.ItemNamed['TARGETS'];
    for IndexTarget := 0 to TargetsNode.Items.Count - 1 do
    begin
      TargetNode := TargetsNode.Items.Item[IndexTarget];
      IsClxProperty := TargetNode.Properties.ItemNamed['ISCLX'];
      IsClx := Assigned(IsClxProperty) and (IsClxProperty.IntValue = 1);
      IndexList := TargetList.Add(TargetNode.Properties.ItemNamed['NAME'].Value);
      if IsClx then
        TargetList.Objects[IndexList] := TObject(1);
      PersonalProperty := TargetNode.Properties.ItemNamed['PNAME'];
      if Assigned(PersonalProperty) then
      begin
        IndexList := TargetList.Add(PersonalProperty.Value);
        if IsClx then
          TargetList.Objects[IndexList] := TObject(1);
      end;
    end;

    AliasesNode := ModelNode.Items.ItemNamed['ALIASES'];
    for IndexAlias := 0 to AliasesNode.Items.Count - 1 do
    begin
      AliasNode := AliasesNode.Items.Item[IndexAlias];
      AliasList.Values[AliasNode.Properties.ItemNamed['NAME'].Value] := AliasNode.Properties.ItemNamed['VALUE'].Value;
    end;

    JvDualListDialogSelect.List1.Clear;
    JvDualListDialogSelect.List2.Clear;

    BuildFileList(PackagePath + 'xml\*.xml', faAnyFile and not faDirectory, PackageList);
    for IndexPackage := 0 to PackageList.Count - 1 do
    begin
      APackageXmlInfo := TPackageXmlInfo.Create(PackagePath + 'xml\' + PackageList.Strings[IndexPackage]);
      PackageList.Objects[IndexPackage] := APackageXmlInfo;
      PackageName := APackageXmlInfo.Name;
      PackageList.Strings[IndexPackage] := PackageName;

      if SelectedPackageNode.Properties.ItemNamed[PackageName].IntValue <> -1 then
        JvDualListDialogSelect.List1.Add(PackageName)
      else
        JvDualListDialogSelect.List2.Add(PackageName);
    end;

    if not JvDualListDialogSelect.Execute then
      Abort;

    SelectedPackageNode.Properties.Clear;
    for IndexPackage := 0 to PackageList.Count - 1 do
    begin
      PackageName := PackageList.Strings[IndexPackage];
      if JvDualListDialogSelect.List1.IndexOf(PackageName) >= 0 then
      begin
        SelectedPackageNode.Properties.Add(PackageName, 1);
        CheckPackage(TPackageXmlInfo(PackageList.Objects[IndexPackage]));
      end;
    end;

    for IndexPackage := 0 to PackageList.Count - 1 do
      PackageList.Objects[IndexPackage].Free;
  finally
    PackageList.Free;
    TargetList.Free;
    AliasList.Free;
  end;

  StatusBar.SimpleText := 'Done.';
end;

procedure TPackageCheckForm.ButtonTargetDefinesClick(Sender: TObject);
var
  DefForm: TDefForm;
begin
  DefForm := TDefForm.Create(Self);
  try
    DefForm.Execute(FTargetsInfo);
  finally
    DefForm.Free;
  end;
end;

procedure TPackageCheckForm.FormCreate(Sender: TObject);
var
  ModelsNode, GUINode, FormNode, TargetsNode: TJclSimpleXMLElem;
  Index: Integer;
begin
  FXMLFileName := ChangeFileExt(Application.ExeName, '.xml');

  FSettings := TJclSimpleXML.Create;
  if FileExists(FXMLFileName) then
    FSettings.LoadFromFile(FXMLFileName)
  else
    FSettings.Root.Name := 'PACKAGECHECK';
  FSettings.Options := FSettings.Options + [sxoAutoCreate];
  GUINode := FSettings.Root.Items.ItemNamed['GUI'];
  TargetsNode := FSettings.Root.Items.ItemNamed['TARGETS'];

  FTargetsInfo := TTargetsInfo.Create;
  FTargetsInfo.LoadFromXMLElem(TargetsNode);

  FPGSettings := TJclSimpleXML.Create;
  FPGSettings.LoadFromFile(PathAddSeparator(ExtractFilePath(Application.ExeName)) + 'PGEdit.xml');
  FPGSettings.Options := FPGSettings.Options - [sxoAutoCreate];

  ComboBoxModel.Items.Clear;
  ModelsNode := FPGSettings.Root.Items.ItemNamed['MODELS'];
  for Index := 0 to ModelsNode.Items.Count - 1 do
    ComboBoxModel.Items.Add(ModelsNode.Items.Item[Index].Properties.ItemNamed['NAME'].Value);

  ComboBoxModel.ItemIndex := ComboBoxModel.Items.IndexOf(GuiNode.Items.ItemNamed['MODEL'].Value);
  FormNode := GuiNode.Items.ItemNamed['FORM'];
  SetBounds(FormNode.Properties.IntValue('LEFT', Left),
            FormNode.Properties.IntValue('TOP', Top),
            FormNode.Properties.IntValue('WIDTH', Width),
            FormNode.Properties.IntValue('HEIGHT', Height));
  CheckBoxCLX.Checked := GUINode.Items.ItemNamed['CLX'].Properties.ItemNamed['ENABLED'].BoolValue;
  CheckBoxHaltOnError.Checked := GUINode.Items.ItemNamed['HALT'].Properties.ItemNamed['ENABLED'].BoolValue;
end;

procedure TPackageCheckForm.FormDestroy(Sender: TObject);
var
  GUINode, TargetsNode, FormNode: TJclSimpleXMLElem;
begin
  GUINode := FSettings.Root.Items.ItemNamed['GUI'];
  GUINode.Items.ItemNamed['MODEL'].Value := ComboBoxModel.Text;
  FormNode := GuiNode.Items.ItemNamed['FORM'];
  FormNode.Properties.ItemNamed['LEFT'].IntValue := Left;
  FormNode.Properties.ItemNamed['TOP'].IntValue := Top;
  FormNode.Properties.ItemNamed['WIDTH'].IntValue := Width;
  FormNode.Properties.ItemNamed['HEIGHT'].IntValue := Height;
  GUINode.Items.ItemNamed['CLX'].Properties.ItemNamed['ENABLED'].BoolValue := CheckBoxCLX.Checked;
  GUINode.Items.ItemNamed['HALT'].Properties.ItemNamed['ENABLED'].BoolValue := CheckBoxHaltOnError.Checked;

  TargetsNode := FSettings.Root.Items.ItemNamed['TARGETS'];
  TargetsNode.Items.Clear;
  FTargetsInfo.SaveToXMLElem(TargetsNode);
  FTargetsInfo.Free;

  FSettings.SaveToFile(FXMLFileName);
  FSettings.Free;
  FPGSettings.Free; // we don't save changes in the package generator config file
end;

end.
