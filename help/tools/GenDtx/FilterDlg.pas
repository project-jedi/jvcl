unit FilterDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,

  ParserTypes, ExtCtrls, ImgList, JvComponent, JvLabel, ComCtrls,
  Mask, JvMaskEdit, JvSpin, ItemFilter;

type
  TMorePage = (mpProperty, mpMethodFunction, mpMethodProcedure, mpFunction,
    mpProcedure, mpClass, mpNone);

  TDirectiveLabelArray = array[TDirective] of TJvLabel;
  TScopeLabelArray = array[TClassVisibility] of TJvLabel;

  TfrmFilter = class(TForm)
    chbClasses: TCheckBox;
    chbConst: TCheckBox;
    chbDispInterface: TCheckBox;
    chbFunction: TCheckBox;
    chbFunctionType: TCheckBox;
    chbInterface: TCheckBox;
    chbMethodFunc: TCheckBox;
    chbMethodProc: TCheckBox;
    chbProcedure: TCheckBox;
    chbProcedureType: TCheckBox;
    chbProperty: TCheckBox;
    chbRecord: TCheckBox;
    chbResourcestring: TCheckBox;
    chbEnum: TCheckBox;
    chbType: TCheckBox;
    chbVar: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    actOK: TAction;
    actCancel: TAction;
    Bevel2: TBevel;
    rgrDuplicatesOrUnique: TRadioGroup;
    ImageList1: TImageList;
    pgcMore: TPageControl;
    tshProperty: TTabSheet;
    jlbProperty_Inherited: TJvLabel;
    jlbProperty_Array: TJvLabel;
    Bevel1: TBevel;
    lblProperty_SpecifierIndex: TJvLabel;
    lblProperty_SpecifierRead: TJvLabel;
    lblProperty_SpecifierWrite: TJvLabel;
    lblProperty_SpecifierStored: TJvLabel;
    lblProperty_SpecifierDefault: TJvLabel;
    lblProperty_SpecifierNoDefault: TJvLabel;
    lblProperty_SpecifierImplements: TJvLabel;
    Label1: TLabel;
    Bevel3: TBevel;
    lblMore_Property: TJvLabel;
    lblMore_MethodFunction: TJvLabel;
    lblMore_MethodProcedure: TJvLabel;
    lblMore_Function: TJvLabel;
    lblMore_Procedure: TJvLabel;
    tshProcedureMethod: TTabSheet;
    tshFunctionMethod: TTabSheet;
    jlbProcedureMethod_ClassMethod: TJvLabel;
    pnlProcedureMethod_Directives: TPanel;
    Label2: TLabel;
    pnlFunctionMethod_Scope: TPanel;
    Label4: TLabel;
    tshNone: TTabSheet;
    tshProcedure: TTabSheet;
    pnlProcedureMethod_Scope: TPanel;
    Label7: TLabel;
    pnlProperty_Scope: TPanel;
    Label8: TLabel;
    jlbProcedureMethod_Constructor: TJvLabel;
    jlbProcedureMethod_Destructor: TJvLabel;
    pnlFunctionMethod_Directives: TPanel;
    Label3: TLabel;
    jlbFunctionMethod_ClassMethod: TJvLabel;
    pnlProcedure_Directives: TPanel;
    Label5: TLabel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    JvSpinEdit3: TJvSpinEdit;
    JvSpinEdit4: TJvSpinEdit;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    JvSpinEdit5: TJvSpinEdit;
    JvSpinEdit6: TJvSpinEdit;
    chbProcMeth_MinParamCount: TCheckBox;
    sedProcMeth_MinParamCount: TJvSpinEdit;
    chbProcMeth_MaxParamCount: TCheckBox;
    sedProcMeth_MaxParamCount: TJvSpinEdit;
    tshClass: TTabSheet;
    Label9: TLabel;
    edtAncestor: TEdit;
    jlbMore_Class: TJvLabel;
    lsbPropertyIn: TListBox;
    Button3: TButton;
    Button4: TButton;
    actAddIn: TAction;
    actDeleteIn: TAction;
    edtPropertyIn: TEdit;
    grbLegend: TGroupBox;
    JvLabel1: TJvLabel;
    JvLabel2: TJvLabel;
    JvLabel3: TJvLabel;
    JvLabel4: TJvLabel;
    rgrSearchSection: TRadioGroup;
    chbSearchInterface: TCheckBox;
    chbSearchImplementation: TCheckBox;
    tshFunction: TTabSheet;
    pnlFunction_Directives: TPanel;
    Label6: TLabel;
    JvSpinEdit1: TJvSpinEdit;
    JvSpinEdit2: TJvSpinEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure FourStateLabelClick(Sender: TObject);
    procedure TriStateLabelClick(Sender: TObject);
    procedure OnMoreClick(Sender: TObject);
    procedure chbPropertyEnter(Sender: TObject);
    procedure chbMethodFuncEnter(Sender: TObject);
    procedure chbMethodProcEnter(Sender: TObject);
    procedure chbFunctionEnter(Sender: TObject);
    procedure chbProcedureEnter(Sender: TObject);
    procedure ItemWithoutMorePageEnter(Sender: TObject);
    procedure BooleanLabelClick(Sender: TObject);
    procedure chbClassesEnter(Sender: TObject);
    procedure actDeleteInExecute(Sender: TObject);
    procedure actDeleteInUpdate(Sender: TObject);
    procedure actAddInExecute(Sender: TObject);
    procedure actAddInUpdate(Sender: TObject);
  private
    FMethodFunction_DirectiveLabels: TDirectiveLabelArray;
    FMethodFunction_ScopeLabels: TScopeLabelArray;
    FMethodProcedure_DirectiveLabels: TDirectiveLabelArray;
    FMethodProcedure_ScopeLabels: TScopeLabelArray;
    FFunction_DirectiveLabels: TDirectiveLabelArray;
    FProcedure_DirectiveLabels: TDirectiveLabelArray;
    FProperty_ScopeLabels: TScopeLabelArray;

    procedure CreateScopeLabels(APanel: TPanel; var AArray: TScopeLabelArray);
    procedure CreateDirectiveLabels(APanel: TPanel; var AArray: TDirectiveLabelArray);

    function GetFourState(const InExcludeSet, InIncludeSet, InMinOneSet: Boolean): TFourState;
    function GetFourStateCtrl(ALabel: TJvLabel): TFourState;
    function GetLabel(const APropertySpecifier: TPropertySpecifier): TJvLabel; overload;
    function GetScopeCtrls(const AArray: TScopeLabelArray): TClassVisibilities;
    function GetTriStateCtrl(ALabel: TJvLabel): TTriState;
    function GetIncludeDirectiveCtrls(const AArray: TDirectiveLabelArray): TDirectives;
    function GetIncludeOneOfDirectiveCtrls(const AArray: TDirectiveLabelArray): TDirectives;
    function GetExcludeDirectiveCtrls(const AArray: TDirectiveLabelArray): TDirectives;

    procedure SetDirectiveCtrls(const MustInclude, MustIncludeOneOf, MustExclude: TDirectives; const AArray:
      TDirectiveLabelArray);
    procedure SetFourStateCtrl(ALabel: TJvLabel; const AState: TFourState);
    procedure SetScopeCtrls(const Scope: TClassVisibilities; const AArray: TScopeLabelArray);
    procedure SetTriStateCtrl(ALabel: TJvLabel; const AState: TTriState);
  protected
    procedure ChangeToMorePage(const AMorePage: TMorePage);
    function GetMorePage(AObject: TObject): TMorePage;

    procedure ConstructControls;
    procedure InitControls;
    procedure CtrlsToData(AFilter: TItemFilter);
    procedure DataToCtrls(AFilter: TItemFilter);
  public
    class function Execute(AFilter: TItemFilter): Boolean;
  end;

implementation

{$R *.dfm}
const
  { diAbstract, diCdecl, diDynamic, diObject, diOf, diOverload,
    diOverride, diPascal, diRegister, diReintroduce, diSafecall, diStdcall,
    diVirtual, diAssembler, diDeprecated, diPlatform, diForward }

  CDirectiveStr: array[TDirective] of string = (
    'abstract', 'cdecl', 'dynamic', 'of object', '', 'overload',
    'override', 'pascal', 'register', 'reintroduce', 'safecall', 'stdcall',
    'virtual', 'assembler', 'deprecated', 'platform', 'forward', 'export', 'far',
    'varargs', 'message', 'external', 'near'
    );

  { inPrivate, inProtected, inPublic, inPublished }

  CClassVisibilityStr: array[TClassVisibility] of string = (
    'Private', 'Protected', 'Public', 'Published'
    );

  CImageIndexBoolean: array[Boolean] of TImageIndex = (0, 3);
  CImageIndexFourState: array[TFourState] of TImageIndex = (0, 1, 2, 3);
  CImageIndexTriState: array[TTriState] of TImageIndex = (0, 1, 3);

//=== Local procedures =======================================================

function ImageIndexToFourState(const AImageIndex: TImageIndex): TFourState;
begin
  if CImageIndexFourState[fsNo] = AImageIndex then
    Result := fsNo
  else
    if CImageIndexFourState[fsOneOf] = AImageIndex then
    Result := fsOneOf
  else
    if CImageIndexFourState[fsYes] = AImageIndex then
    Result := fsYes
  else
    Result := fsDontCare;
end;

function ImageIndexToTriState(const AImageIndex: TImageIndex): TTriState;
begin
  if CImageIndexTriState[tsNo] = AImageIndex then
    Result := tsNo
  else
    if CImageIndexTriState[tsYes] = AImageIndex then
    Result := tsYes
  else
    Result := tsDontCare;
end;

function ImageIndexToBoolean(const AImageIndex: TImageIndex): Boolean;
begin
  Result := CImageIndexBoolean[True] = AImageIndex;
end;

//=== TfrmFilter =============================================================

procedure TfrmFilter.actAddInExecute(Sender: TObject);
begin
  lsbPropertyIn.Items.Add(edtPropertyIn.Text);
end;

procedure TfrmFilter.actAddInUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := edtPropertyIn.Text <> '';
end;

procedure TfrmFilter.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmFilter.actDeleteInExecute(Sender: TObject);
var
  I: Integer;
begin
  I := lsbPropertyIn.ItemIndex;
  if I >= 0 then
  begin
    lsbPropertyIn.Items.Delete(I);
    if I >= lsbPropertyIn.Count then
      Dec(I);
    lsbPropertyIn.ItemIndex := I;
  end;
end;

procedure TfrmFilter.actDeleteInUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := lsbPropertyIn.ItemIndex >= 0;
end;

procedure TfrmFilter.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmFilter.BooleanLabelClick(Sender: TObject);
begin
  if Sender is TJvLabel then
    with Sender as TJvLabel do
      ImageIndex := CImageIndexBoolean[not ImageIndexToBoolean(ImageIndex)];
end;

procedure TfrmFilter.ChangeToMorePage(const AMorePage: TMorePage);
begin
  case AMorePage of
    mpProperty: pgcMore.ActivePage := tshProperty;
    mpMethodFunction: pgcMore.ActivePage := tshFunctionMethod;
    mpMethodProcedure: pgcMore.ActivePage := tshProcedureMethod;
    mpFunction: pgcMore.ActivePage := tshFunction;
    mpProcedure: pgcMore.ActivePage := tshProcedure;
    mpClass: pgcMore.ActivePage := tshClass;
  else
    pgcMore.ActivePage := tshNone;
  end;
end;

procedure TfrmFilter.chbClassesEnter(Sender: TObject);
begin
  ChangeToMorePage(mpClass);
end;

procedure TfrmFilter.chbFunctionEnter(Sender: TObject);
begin
  ChangeToMorePage(mpFunction);
end;

procedure TfrmFilter.chbMethodFuncEnter(Sender: TObject);
begin
  ChangeToMorePage(mpMethodFunction);
end;

procedure TfrmFilter.chbMethodProcEnter(Sender: TObject);
begin
  ChangeToMorePage(mpMethodProcedure);
end;

procedure TfrmFilter.chbProcedureEnter(Sender: TObject);
begin
  ChangeToMorePage(mpProcedure);
end;

procedure TfrmFilter.chbPropertyEnter(Sender: TObject);
begin
  ChangeToMorePage(mpProperty);
end;

procedure TfrmFilter.ConstructControls;
begin
  CreateDirectiveLabels(pnlProcedureMethod_Directives, FMethodProcedure_DirectiveLabels);
  CreateDirectiveLabels(pnlFunctionMethod_Directives, FMethodFunction_DirectiveLabels);
  CreateDirectiveLabels(pnlProcedure_Directives, FProcedure_DirectiveLabels);
  CreateDirectiveLabels(pnlFunction_Directives, FFunction_DirectiveLabels);
  CreateScopeLabels(pnlProcedureMethod_Scope, FMethodProcedure_ScopeLabels);
  CreateScopeLabels(pnlFunctionMethod_Scope, FMethodFunction_ScopeLabels);
  CreateScopeLabels(pnlProperty_Scope, FProperty_ScopeLabels);
end;

procedure TfrmFilter.CreateDirectiveLabels(APanel: TPanel;
  var AArray: TDirectiveLabelArray);
var
  LLeft: Integer;
  LTop: Integer;
  Directive: TDirective;
  MaxWidth: Integer;
begin
  LLeft := 7;
  LTop := 37;
  MaxWidth := 0;

  for Directive := Low(TDirective) to High(TDirective) do
    if Directive <> diOf then
    begin
      AArray[Directive] := TJvLabel.Create(Self);
      with AArray[Directive] do
      begin
        //Name := 'jlbProcMeth_DirecAssembler';
        Parent := APanel;
        SetBounds(LLeft, LTop, 73, 17);
        AutoSize := True;
        Caption := CDirectiveStr[Directive];
        OnClick := FourStateLabelClick;
        AutoOpenURL := False;
        Images := ImageList1;
        ImageIndex := -1;
        if Width > MaxWidth then
          MaxWidth := Width;
      end;

      Inc(LTop, 18);
      if LTop + 17 > APanel.Height then
      begin
        LTop := 19;
        LLeft := MaxWidth + LLeft + 5;
        MaxWidth := 0;
      end;
    end;
end;

procedure TfrmFilter.CreateScopeLabels(APanel: TPanel;
  var AArray: TScopeLabelArray);
var
  LLeft: Integer;
  LTop: Integer;
  Scope: TClassVisibility;
begin
  LLeft := 7;
  LTop := 37;

  for Scope := Low(TClassVisibility) to High(TClassVisibility) do
  begin
    AArray[Scope] := TJvLabel.Create(Self);
    with AArray[Scope] do
    begin
      //Name := 'jlbProcMeth_DirecAssembler';
      Parent := APanel;
      SetBounds(LLeft, LTop, 73, 17);
      AutoSize := False;
      Caption := CClassVisibilityStr[Scope];
      OnClick := BooleanLabelClick;
      AutoOpenURL := False;
      Images := ImageList1;
      ImageIndex := -1;
    end;

    Inc(LTop, 18);
  end;
end;

procedure TfrmFilter.CtrlsToData(AFilter: TItemFilter);

  procedure DoProperty;
  var
    PropertySpecifier: TPropertySpecifier;
  begin
    with AFilter.PropertyFilter do
    begin
      ShowInherited := GetTriStateCtrl(jlbProperty_Inherited);
      ShowArray := GetTriStateCtrl(jlbProperty_Array);
      Scope := GetScopeCtrls(FProperty_ScopeLabels);
      MustExcludeSpecifiers := [];
      MustIncludeSpecifiers := [];
      MustIncludeOneOfSpecifiers := [];
      for PropertySpecifier := Low(TPropertySpecifier) to High(PropertySpecifier) do
        case GetFourStateCtrl(GetLabel(PropertySpecifier)) of
          fsNo: MustExcludeSpecifiers := MustExcludeSpecifiers + [PropertySpecifier];
          fsDontCare: ;
          fsOneOf: MustIncludeOneOfSpecifiers := MustIncludeOneOfSpecifiers + [PropertySpecifier];
          fsYes: MustIncludeSpecifiers := MustIncludeSpecifiers + [PropertySpecifier];
        end;

      InList.Assign(lsbPropertyIn.Items);
    end;
  end;

  procedure DoMethodProcedure;
  begin
    with AFilter.MethodProcedureFilter do
    begin
      ShowConstructor := GetTriStateCtrl(jlbProcedureMethod_Constructor);
      ShowDestructor := GetTriStateCtrl(jlbProcedureMethod_Destructor);
      ShowClassMethod := GetTriStateCtrl(jlbProcedureMethod_ClassMethod);
      Scope := GetScopeCtrls(FMethodProcedure_ScopeLabels);
      MustExcludeDirectives := GetExcludeDirectiveCtrls(FMethodProcedure_DirectiveLabels);
      MustIncludeOneOfDirectives := GetIncludeOneOfDirectiveCtrls(FMethodProcedure_DirectiveLabels);
      MustIncludeDirectives := GetIncludeDirectiveCtrls(FMethodProcedure_DirectiveLabels);
    end;
  end;

  procedure DoMethodFunction;
  begin
    with AFilter.MethodFunctionFilter do
    begin
      ShowClassMethod := GetTriStateCtrl(jlbFunctionMethod_ClassMethod);
      Scope := GetScopeCtrls(FMethodFunction_ScopeLabels);
      MustExcludeDirectives := GetExcludeDirectiveCtrls(FMethodFunction_DirectiveLabels);
      MustIncludeOneOfDirectives := GetIncludeOneOfDirectiveCtrls(FMethodFunction_DirectiveLabels);
      MustIncludeDirectives := GetIncludeDirectiveCtrls(FMethodFunction_DirectiveLabels);
    end;
  end;

  procedure DoProcedure;
  begin
    with AFilter.ProcedureFilter do
    begin
      MustExcludeDirectives := GetExcludeDirectiveCtrls(FProcedure_DirectiveLabels);
      MustIncludeOneOfDirectives := GetIncludeOneOfDirectiveCtrls(FProcedure_DirectiveLabels);
      MustIncludeDirectives := GetIncludeDirectiveCtrls(FProcedure_DirectiveLabels);
    end;
  end;

  procedure DoFunction;
  begin
    with AFilter.FunctionFilter do
    begin
      MustExcludeDirectives := GetExcludeDirectiveCtrls(FFunction_DirectiveLabels);
      MustIncludeOneOfDirectives := GetIncludeOneOfDirectiveCtrls(FFunction_DirectiveLabels);
      MustIncludeDirectives := GetIncludeDirectiveCtrls(FFunction_DirectiveLabels);
    end;
  end;

  procedure DoClass;
  begin
    with AFilter.ClassFilter do
    begin
      DescendantOf := edtAncestor.Text;
    end;
  end;
begin
  with AFilter do
  begin
    DelphiTypes := [];
    if chbClasses.Checked then
      DelphiTypes := DelphiTypes + [dtClass];
    if chbConst.Checked then
      DelphiTypes := DelphiTypes + [dtConst];
    if chbDispInterface.Checked then
      DelphiTypes := DelphiTypes + [dtDispInterface];
    if chbFunction.Checked then
      DelphiTypes := DelphiTypes + [dtFunction];
    if chbFunctionType.Checked then
      DelphiTypes := DelphiTypes + [dtFunctionType];
    if chbInterface.Checked then
      DelphiTypes := DelphiTypes + [dtInterface];
    if chbMethodFunc.Checked then
      DelphiTypes := DelphiTypes + [dtMethodFunc];
    if chbMethodProc.Checked then
      DelphiTypes := DelphiTypes + [dtMethodProc];
    if chbProcedure.Checked then
      DelphiTypes := DelphiTypes + [dtProcedure];
    if chbProcedureType.Checked then
      DelphiTypes := DelphiTypes + [dtProcedureType];
    if chbProperty.Checked then
      DelphiTypes := DelphiTypes + [dtProperty];
    if chbRecord.Checked then
      DelphiTypes := DelphiTypes + [dtRecord];
    if chbResourcestring.Checked then
      DelphiTypes := DelphiTypes + [dtResourcestring];
    if chbEnum.Checked then
      DelphiTypes := DelphiTypes + [dtEnum];
    if chbType.Checked then
      DelphiTypes := DelphiTypes + [dtType];
    if chbVar.Checked then
      DelphiTypes := DelphiTypes + [dtVar];

    Duplicates := TDuplicatesType(rgrDuplicatesOrUnique.ItemIndex);
    SearchInInterfaceSection := chbSearchInterface.Checked;
    SearchInImplementationSection := chbSearchImplementation.Checked;
  end;

  DoProperty;
  DoMethodProcedure;
  DoMethodFunction;
  DoProcedure;
  DoFunction;
  DoClass;
end;

procedure TfrmFilter.DataToCtrls(AFilter: TItemFilter);

  procedure DoProperty;
  var
    PropertySpecifier: TPropertySpecifier;
  begin
    with AFilter.PropertyFilter do
    begin
      SetTriStateCtrl(jlbProperty_Inherited, ShowInherited);
      SetTriStateCtrl(jlbProperty_Array, ShowArray);
      SetScopeCtrls(Scope, FProperty_ScopeLabels);
      for PropertySpecifier := Low(TPropertySpecifier) to High(PropertySpecifier) do
        SetFourStateCtrl(GetLabel(PropertySpecifier),
          GetFourState(
          PropertySpecifier in MustExcludeSpecifiers,
          PropertySpecifier in MustIncludeSpecifiers,
          PropertySpecifier in MustIncludeOneOfSpecifiers
          ));
      lsbPropertyIn.Items.Assign(InList)
    end;
  end;

  procedure DoMethodProcedure;
  begin
    with AFilter.MethodProcedureFilter do
    begin
      SetTriStateCtrl(jlbProcedureMethod_Constructor, ShowConstructor);
      SetTriStateCtrl(jlbProcedureMethod_Destructor, ShowDestructor);
      SetTriStateCtrl(jlbProcedureMethod_ClassMethod, ShowClassMethod);
      SetScopeCtrls(Scope, FMethodProcedure_ScopeLabels);
      SetDirectiveCtrls(
        MustIncludeDirectives,
        MustIncludeOneOfDirectives,
        MustExcludeDirectives,
        FMethodProcedure_DirectiveLabels);
    end;
  end;

  procedure DoMethodFunction;
  begin
    with AFilter.MethodFunctionFilter do
    begin
      SetTriStateCtrl(jlbFunctionMethod_ClassMethod, ShowClassMethod);
      SetScopeCtrls(Scope, FMethodFunction_ScopeLabels);
      SetDirectiveCtrls(
        MustIncludeDirectives,
        MustIncludeOneOfDirectives,
        MustExcludeDirectives,
        FMethodFunction_DirectiveLabels);
    end;
  end;

  procedure DoProcedure;
  begin
    with AFilter.ProcedureFilter do
    begin
      SetDirectiveCtrls(
        MustIncludeDirectives,
        MustIncludeOneOfDirectives,
        MustExcludeDirectives,
        FProcedure_DirectiveLabels);
    end;
  end;

  procedure DoFunction;
  begin
    with AFilter.FunctionFilter do
    begin
      SetDirectiveCtrls(
        MustIncludeDirectives,
        MustIncludeOneOfDirectives,
        MustExcludeDirectives,
        FFunction_DirectiveLabels);
    end;
  end;
  procedure DoClass;
  begin
    with AFilter.ClassFilter do
    begin
      edtAncestor.Text := DescendantOf;
    end;
  end;
begin
  with AFilter do
  begin
    chbClasses.Checked := dtClass in DelphiTypes;
    chbConst.Checked := dtConst in DelphiTypes;
    chbDispInterface.Checked := dtDispInterface in DelphiTypes;
    chbFunction.Checked := dtFunction in DelphiTypes;
    chbFunctionType.Checked := dtFunctionType in DelphiTypes;
    chbInterface.Checked := dtInterface in DelphiTypes;
    chbMethodFunc.Checked := dtMethodFunc in DelphiTypes;
    chbMethodProc.Checked := dtMethodProc in DelphiTypes;
    chbProcedure.Checked := dtProcedure in DelphiTypes;
    chbProcedureType.Checked := dtProcedureType in DelphiTypes;

    chbProperty.Checked := dtProperty in DelphiTypes;

    chbRecord.Checked := dtRecord in DelphiTypes;
    chbResourcestring.Checked := dtResourcestring in DelphiTypes;
    chbEnum.Checked := dtEnum in DelphiTypes;
    chbType.Checked := DtType in DelphiTypes;
    chbVar.Checked := dtVar in DelphiTypes;

    rgrDuplicatesOrUnique.ItemIndex := Integer(Duplicates);
    chbSearchInterface.Checked := SearchInInterfaceSection;
    chbSearchImplementation.Checked := SearchInImplementationSection;
  end;
  DoProperty;
  DoMethodProcedure;
  DoMethodFunction;
  DoProcedure;
  DoFunction;
  DoClass;
end;

class function TfrmFilter.Execute(AFilter: TItemFilter): Boolean;
begin
  with TfrmFilter.Create(Application) do
  try
    ConstructControls;
    InitControls;
    DataToCtrls(AFilter);
    Result := ShowModal = mrOk;
    //if Result then
    CtrlsToData(AFilter);
  finally
    Free;
  end;
end;

procedure TfrmFilter.FourStateLabelClick(Sender: TObject);
var
  FourState: TFourState;
begin
  if Sender is TJvLabel then
    with Sender as TJvLabel do
    begin
      FourState := ImageIndexToFourState(ImageIndex);
      if FourState = High(TFourState) then
        FourState := Low(TFourState)
      else
        FourState := Succ(FourState);
      ImageIndex := CImageIndexFourState[FourState];
    end;
end;

function TfrmFilter.GetExcludeDirectiveCtrls(
  const AArray: TDirectiveLabelArray): TDirectives;
var
  Directive: TDirective;
  FourState: TFourState;
begin
  Result := [];

  for Directive := Low(TDirective) to High(TDirective) do
    if Directive <> diOf then
    begin
      FourState := ImageIndexToFourState(AArray[Directive].ImageIndex);
      if FourState = fsNo then Include(Result, Directive);
    end;
end;

function TfrmFilter.GetFourState(const InExcludeSet,
  InIncludeSet, InMinOneSet: Boolean): TFourState;
begin
  if InExcludeSet then
    Result := fsNo
  else
    if InIncludeSet then
    Result := fsYes
  else
    if InMinOneSet then
    Result := fsOneOf
  else
    Result := fsDontCare;
end;

function TfrmFilter.GetFourStateCtrl(ALabel: TJvLabel): TFourState;
begin
  Result := ImageIndexToFourState(ALabel.ImageIndex);
end;

function TfrmFilter.GetIncludeDirectiveCtrls(
  const AArray: TDirectiveLabelArray): TDirectives;
var
  Directive: TDirective;
  FourState: TFourState;
begin
  Result := [];

  for Directive := Low(TDirective) to High(TDirective) do
    if Directive <> diOf then
    begin
      FourState := ImageIndexToFourState(AArray[Directive].ImageIndex);
      if FourState = fsYes then
        Include(Result, Directive);
    end;
end;

function TfrmFilter.GetIncludeOneOfDirectiveCtrls(
  const AArray: TDirectiveLabelArray): TDirectives;
var
  Directive: TDirective;
  FourState: TFourState;
begin
  Result := [];

  for Directive := Low(TDirective) to High(TDirective) do
    if Directive <> diOf then
    begin
      FourState := ImageIndexToFourState(AArray[Directive].ImageIndex);
      if FourState = fsOneOf then Include(Result, Directive);
    end;
end;

function TfrmFilter.GetLabel(
  const APropertySpecifier: TPropertySpecifier): TJvLabel;
begin
  case APropertySpecifier of
    psIndex: Result := lblProperty_SpecifierIndex;
    psRead: Result := lblProperty_SpecifierRead;
    psWrite: Result := lblProperty_SpecifierWrite;
    psStored: Result := lblProperty_SpecifierStored;
    psDefault: Result := lblProperty_SpecifierDefault;
    psNoDefault: Result := lblProperty_SpecifierNoDefault;
    psImplements: Result := lblProperty_SpecifierImplements;
  else
    raise Exception.Create('Unknown');
  end;
end;

function TfrmFilter.GetMorePage(AObject: TObject): TMorePage;
begin
  if AObject = lblMore_Property then
    Result := mpProperty
  else
    if AObject = lblMore_MethodFunction then
    Result := mpMethodFunction
  else
    if AObject = lblMore_MethodProcedure then
    Result := mpMethodProcedure
  else
    if AObject = lblMore_Function then
    Result := mpFunction
  else
    if AObject = lblMore_Procedure then
    Result := mpProcedure
  else
    if AObject = jlbMore_Class then
    Result := mpClass
  else
    raise Exception.Create('Unknown');
end;

function TfrmFilter.GetScopeCtrls(const AArray: TScopeLabelArray): TClassVisibilities;
var
  ClassVisibility: TClassVisibility;
begin
  Result := [];
  for ClassVisibility := Low(TClassVisibility) to High(TClassVisibility) do
    if AArray[ClassVisibility].ImageIndex = CImageIndexBoolean[True] then
      Include(Result, ClassVisibility);
end;

function TfrmFilter.GetTriStateCtrl(ALabel: TJvLabel): TTriState;
begin
  Result := ImageIndexToTriState(ALabel.ImageIndex);
end;

procedure TfrmFilter.InitControls;
var
  Duplicate: TDuplicatesType;
begin
  rgrDuplicatesOrUnique.Items.BeginUpdate;
  try
    for Duplicate := Low(TDuplicatesType) to High(TDuplicatesType) do
      rgrDuplicatesOrUnique.Items.Add(CDuplicatesTypeStr[Duplicate]);
  finally
    rgrDuplicatesOrUnique.Items.EndUpdate;
  end;
end;

procedure TfrmFilter.ItemWithoutMorePageEnter(Sender: TObject);
begin
  ChangeToMorePage(mpNone);
end;

procedure TfrmFilter.OnMoreClick(Sender: TObject);
begin
  ChangeToMorePage(GetMorePage(Sender));
end;

procedure TfrmFilter.SetDirectiveCtrls(const MustInclude, MustIncludeOneOf,
  MustExclude: TDirectives; const AArray: TDirectiveLabelArray);
var
  Directive: TDirective;
  FourState: TFourState;
begin
  for Directive := Low(TDirective) to High(TDirective) do
    if Directive <> diOf then
    begin
      if Directive in MustInclude then
        FourState := fsYes
      else
        if Directive in MustExclude then
        FourState := fsNo
      else
        if Directive in MustIncludeOneOf then
        FourState := fsOneOf
      else
        FourState := fsDontCare;

      AArray[Directive].ImageIndex := CImageIndexFourState[FourState];
    end;
end;

procedure TfrmFilter.SetFourStateCtrl(ALabel: TJvLabel;
  const AState: TFourState);
begin
  ALabel.ImageIndex := CImageIndexFourState[AState];
end;

procedure TfrmFilter.SetScopeCtrls(const Scope: TClassVisibilities;
  const AArray: TScopeLabelArray);
var
  ClassVisibility: TClassVisibility;
begin
  for ClassVisibility := Low(TClassVisibility) to High(TClassVisibility) do
    AArray[ClassVisibility].ImageIndex := CImageIndexBoolean[ClassVisibility in Scope];
end;

procedure TfrmFilter.SetTriStateCtrl(ALabel: TJvLabel;
  const AState: TTriState);
begin
  ALabel.ImageIndex := CImageIndexTriState[AState];
end;

procedure TfrmFilter.TriStateLabelClick(Sender: TObject);
var
  TriState: TTriState;
begin
  if Sender is TJvLabel then
    with Sender as TJvLabel do
    begin
      TriState := ImageIndexToTriState(ImageIndex);
      if TriState = High(TTriState) then
        TriState := Low(TTriState)
      else
        TriState := Succ(TriState);
      ImageIndex := CImageIndexTriState[TriState];
    end;
end;

end.
