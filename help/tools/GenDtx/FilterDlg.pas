unit FilterDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,

  ParserTypes, ExtCtrls, ImgList, JvComponent, JvxCtrls, JvLabel, ComCtrls,
  Mask, JvMaskEdit, JvSpin;

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
    tshFunction: TTabSheet;
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
    jlbProcedure_ClassMethod: TJvLabel;
    pnlFunction_Directives: TPanel;
    Label6: TLabel;
    jlbFunction_ClassMethod: TJvLabel;
    JvSpinEdit1: TJvSpinEdit;
    JvSpinEdit2: TJvSpinEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
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
    //AFilter: TFilterData;

    FMethodFunction_DirectiveLabels: TDirectiveLabelArray;
    FMethodFunction_ScopeLabels: TScopeLabelArray;
    FMethodProcedure_DirectiveLabels: TDirectiveLabelArray;
    FMethodProcedure_ScopeLabels: TScopeLabelArray;
    FFunction_DirectiveLabels: TDirectiveLabelArray;
    FProcedure_DirectiveLabels: TDirectiveLabelArray;
    FProperty_ScopeLabels: TScopeLabelArray;

    procedure CreateScopeLabels(APanel: TPanel; var AArray: TScopeLabelArray);
    procedure CreateDirectiveLabels(APanel: TPanel; var AArray: TDirectiveLabelArray);

    procedure SetScopeCtrls(const Scope: TClassVisibilities; const AArray: TScopeLabelArray);
    procedure GetScopeCtrls(var Scope: TClassVisibilities; const AArray: TScopeLabelArray);
    procedure SetDirectiveCtrls(const MustInclude, MustIncludeOneOf, MustExclude: TDirectives; const AArray:
      TDirectiveLabelArray);
    procedure GetDirectiveCtrls(var MustInclude, MustIncludeOneOf, MustExclude: TDirectives; const AArray:
      TDirectiveLabelArray);

    function GetLabel(const APropertySpecifier: TPropertySpecifier): TJvLabel; overload;
    function GetFourState(const InExcludeSet, InIncludeSet, InMinOneSet: Boolean): TFourState;
    procedure SetFourStateCtrl(ALabel: TJvLabel; const AState: TFourState);
    function GetFourStateCtrl(ALabel: TJvLabel): TFourState;
    procedure SetTriStateCtrl(ALabel: TJvLabel; const AState: TTriState);
    function GetTriStateCtrl(ALabel: TJvLabel): TTriState;
  protected
    procedure ChangeToMorePage(const AMorePage: TMorePage);
    function GetMorePage(AObject: TObject): TMorePage;

    procedure ConstructControls;
    procedure CtrlsToData(var AFilter: TFilterData);
    procedure DataToCtrls(const AFilter: TFilterData);
  public
    class function Execute(var AFilter: TFilterData): Boolean;
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
    'virtual', 'assembler', 'deprecated', 'platform', 'forward', 'export'
    );

  { inPrivate, inProtected, inPublic, inPublished }
  CClassVisibilityStr: array[TClassVisibility] of string = (
    'Private', 'Protected', 'Public', 'Published'
    );

  CImageIndexBoolean: array[Boolean] of TImageIndex = (0, 3);
  CImageIndexFourState: array[TFourState] of TImageIndex = (0, 1, 2, 3);
  CImageIndexTriState: array[TTriState] of TImageIndex = (0, 1, 3);

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

procedure TfrmFilter.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmFilter.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

class function TfrmFilter.Execute(var AFilter: TFilterData): Boolean;
begin
  with TfrmFilter.Create(Application) do
  try
    ConstructControls;
    DataToCtrls(AFilter);
    Result := ShowModal = mrOk;
    //if Result then
    CtrlsToData(AFilter);
  finally
    Free;
  end;
end;

procedure TfrmFilter.CtrlsToData(var AFilter: TFilterData);

  procedure DoProperty;
  var
    PropertySpecifier: TPropertySpecifier;
  begin
    with AFilter do
    begin
      RProperty_ShowInherited := GetTriStateCtrl(jlbProperty_Inherited);
      RProperty_ShowArray := GetTriStateCtrl(jlbProperty_Array);
      GetScopeCtrls(RProperty_Scope, FProperty_ScopeLabels);
      RProperty_MustExcludeSpecifiers := [];
      RProperty_MustIncludeSpecifiers := [];
      RProperty_MustIncludeOneOfSpecifiers := [];
      for PropertySpecifier := Low(TPropertySpecifier) to High(PropertySpecifier) do
        case GetFourStateCtrl(GetLabel(PropertySpecifier)) of
          fsNo: Include(RProperty_MustExcludeSpecifiers, PropertySpecifier);
          fsDontCare: ;
          fsOneOf: Include(RProperty_MustIncludeOneOfSpecifiers, PropertySpecifier);
          fsYes: Include(RProperty_MustIncludeSpecifiers, PropertySpecifier);
        end;

      if lsbPropertyIn.Count > 0 then
      begin
        if not Assigned(RProperty_In) then
        begin
          RProperty_In := TStringList.Create;
          RProperty_In.Sorted := True;
          RProperty_In.Duplicates := dupIgnore;
        end;
        RProperty_In.Assign(lsbPropertyIn.Items);

        if RProperty_In.Count <= 0 then
          FreeAndNil(RProperty_In);
      end
      else
        FreeAndNil(RProperty_In);
    end;
  end;

  procedure DoMethodProcedure;
  begin
    with AFilter do
    begin
      RMethodProcedure_ShowConstructor := GetTriStateCtrl(jlbProcedureMethod_Constructor);
      RMethodProcedure_ShowDestructor := GetTriStateCtrl(jlbProcedureMethod_Destructor);
      RMethodProcedure_ShowClassMethod := GetTriStateCtrl(jlbProcedureMethod_ClassMethod);
      GetScopeCtrls(RMethodProcedure_Scope, FMethodProcedure_ScopeLabels);
      GetDirectiveCtrls(
        RMethodProcedure_MustIncludeDirectives,
        RMethodProcedure_MustIncludeOneOfDirectives,
        RMethodProcedure_MustExcludeDirectives,
        FMethodProcedure_DirectiveLabels);
    end;
  end;

  procedure DoMethodFunction;
  begin
    with AFilter do
    begin
      RMethodFunction_ShowClassMethod := GetTriStateCtrl(jlbFunctionMethod_ClassMethod);
      GetScopeCtrls(RMethodFunction_Scope, FMethodFunction_ScopeLabels);
      GetDirectiveCtrls(
        RMethodFunction_MustIncludeDirectives,
        RMethodFunction_MustIncludeOneOfDirectives,
        RMethodFunction_MustExcludeDirectives,
        FMethodFunction_DirectiveLabels);
    end;
  end;

  procedure DoProcedure;
  begin
    with AFilter do
    begin
      RProcedure_ShowClassMethod := GetTriStateCtrl(jlbProcedure_ClassMethod);
      GetDirectiveCtrls(
        RProcedure_MustIncludeDirectives,
        RProcedure_MustIncludeOneOfDirectives,
        RProcedure_MustExcludeDirectives,
        FProcedure_DirectiveLabels);
    end;
  end;

  procedure DoFunction;
  begin
    with AFilter do
    begin
      RFunction_ShowClassMethod := GetTriStateCtrl(jlbFunction_ClassMethod);
      GetDirectiveCtrls(
        RFunction_MustIncludeDirectives,
        RFunction_MustIncludeOneOfDirectives,
        RFunction_MustExcludeDirectives,
        FFunction_DirectiveLabels);
    end;
  end;

  procedure DoClass;
  begin
    with AFilter do
    begin
      RClass_DescendantOf := edtAncestor.Text;
    end;
  end;
begin
  with AFilter do
  begin
    RShow := [];
    if chbClasses.Checked then
      Include(RShow, dtClass);
    if chbConst.Checked then
      Include(RShow, dtConst);
    if chbDispInterface.Checked then
      Include(RShow, dtDispInterface);
    if chbFunction.Checked then
      Include(RShow, dtFunction);
    if chbFunctionType.Checked then
      Include(RShow, dtFunctionType);
    if chbInterface.Checked then
      Include(RShow, dtInterface);
    if chbMethodFunc.Checked then
      Include(RShow, dtMethodFunc);
    if chbMethodProc.Checked then
      Include(RShow, dtMethodProc);
    if chbProcedure.Checked then
      Include(RShow, dtProcedure);
    if chbProcedureType.Checked then
      Include(RShow, dtProcedureType);
    if chbProperty.Checked then
      Include(RShow, dtProperty);
    if chbRecord.Checked then
      Include(RShow, dtRecord);
    if chbResourcestring.Checked then
      Include(RShow, dtResourcestring);
    if chbEnum.Checked then
      Include(RShow, dtEnum);
    if chbType.Checked then
      Include(RShow, dtType);
    if chbVar.Checked then
      Include(RShow, dtVar);

    RDuplicates := TDuplicatesType(rgrDuplicatesOrUnique.ItemIndex);
  end;

  DoProperty;
  DoMethodProcedure;
  DoMethodFunction;
  DoProcedure;
  DoFunction;
  DoClass;
end;

procedure TfrmFilter.DataToCtrls(const AFilter: TFilterData);

  procedure DoProperty;
  var
    PropertySpecifier: TPropertySpecifier;
  begin
    with AFilter do
    begin
      SetTriStateCtrl(jlbProperty_Inherited, RProperty_ShowInherited);
      SetTriStateCtrl(jlbProperty_Array, RProperty_ShowArray);
      SetScopeCtrls(RProperty_Scope, FProperty_ScopeLabels);
      for PropertySpecifier := Low(TPropertySpecifier) to High(PropertySpecifier) do
        SetFourStateCtrl(GetLabel(PropertySpecifier),
          GetFourState(
          PropertySpecifier in RProperty_MustExcludeSpecifiers,
          PropertySpecifier in RProperty_MustIncludeSpecifiers,
          PropertySpecifier in RProperty_MustIncludeOneOfSpecifiers
          ));
      if Assigned(RProperty_In) then
        lsbPropertyIn.Items.Assign(RProperty_In)
      else
        lsbPropertyIn.Clear;
    end;
  end;

  procedure DoMethodProcedure;
  begin
    with AFilter do
    begin
      SetTriStateCtrl(jlbProcedureMethod_Constructor, RMethodProcedure_ShowConstructor);
      SetTriStateCtrl(jlbProcedureMethod_Destructor, RMethodProcedure_ShowDestructor);
      SetTriStateCtrl(jlbProcedureMethod_ClassMethod, RMethodProcedure_ShowClassMethod);
      SetScopeCtrls(RMethodProcedure_Scope, FMethodProcedure_ScopeLabels);
      SetDirectiveCtrls(
        RMethodProcedure_MustIncludeDirectives,
        RMethodProcedure_MustIncludeOneOfDirectives,
        RMethodProcedure_MustExcludeDirectives,
        FMethodProcedure_DirectiveLabels);
    end;
  end;

  procedure DoMethodFunction;
  begin
    with AFilter do
    begin
      SetTriStateCtrl(jlbFunctionMethod_ClassMethod, RMethodFunction_ShowClassMethod);
      SetScopeCtrls(RMethodFunction_Scope, FMethodFunction_ScopeLabels);
      SetDirectiveCtrls(
        RMethodFunction_MustIncludeDirectives,
        RMethodFunction_MustIncludeOneOfDirectives,
        RMethodFunction_MustExcludeDirectives,
        FMethodFunction_DirectiveLabels);
    end;
  end;

  procedure DoProcedure;
  begin
    with AFilter do
    begin
      SetTriStateCtrl(jlbProcedure_ClassMethod, RProcedure_ShowClassMethod);
      SetDirectiveCtrls(
        RProcedure_MustIncludeDirectives,
        RProcedure_MustIncludeOneOfDirectives,
        RProcedure_MustExcludeDirectives,
        FProcedure_DirectiveLabels);
    end;
  end;

  procedure DoFunction;
  begin
    with AFilter do
    begin
      SetTriStateCtrl(jlbFunction_ClassMethod, RFunction_ShowClassMethod);
      SetDirectiveCtrls(
        RFunction_MustIncludeDirectives,
        RFunction_MustIncludeOneOfDirectives,
        RFunction_MustExcludeDirectives,
        FFunction_DirectiveLabels);
    end;
  end;
  procedure DoClass;
  begin
    with AFilter do
    begin
      edtAncestor.Text := RClass_DescendantOf;
    end;
  end;
begin
  with AFilter do
  begin
    chbClasses.Checked := dtClass in RShow;
    chbConst.Checked := dtConst in RShow;
    chbDispInterface.Checked := dtDispInterface in RShow;
    chbFunction.Checked := dtFunction in RShow;
    chbFunctionType.Checked := dtFunctionType in RShow;
    chbInterface.Checked := dtInterface in RShow;
    chbMethodFunc.Checked := dtMethodFunc in RShow;
    chbMethodProc.Checked := dtMethodProc in RShow;
    chbProcedure.Checked := dtProcedure in RShow;
    chbProcedureType.Checked := dtProcedureType in RShow;

    chbProperty.Checked := dtProperty in RShow;

    chbRecord.Checked := dtRecord in RShow;
    chbResourcestring.Checked := dtResourcestring in RShow;
    chbEnum.Checked := dtEnum in RShow;
    chbType.Checked := dtType in RShow;
    chbVar.Checked := dtVar in RShow;

    rgrDuplicatesOrUnique.ItemIndex := Integer(RDuplicates);
  end;
  DoProperty;
  DoMethodProcedure;
  DoMethodFunction;
  DoProcedure;
  DoFunction;
  DoClass;
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

procedure TfrmFilter.SetFourStateCtrl(ALabel: TJvLabel;
  const AState: TFourState);
begin
  ALabel.ImageIndex := CImageIndexFourState[AState];
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

procedure TfrmFilter.OnMoreClick(Sender: TObject);
begin
  ChangeToMorePage(GetMorePage(Sender));
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

procedure TfrmFilter.chbPropertyEnter(Sender: TObject);
begin
  ChangeToMorePage(mpProperty);
end;

procedure TfrmFilter.chbMethodFuncEnter(Sender: TObject);
begin
  ChangeToMorePage(mpMethodFunction);
end;

procedure TfrmFilter.chbMethodProcEnter(Sender: TObject);
begin
  ChangeToMorePage(mpMethodProcedure);
end;

procedure TfrmFilter.chbFunctionEnter(Sender: TObject);
begin
  ChangeToMorePage(mpFunction);
end;

procedure TfrmFilter.chbProcedureEnter(Sender: TObject);
begin
  ChangeToMorePage(mpProcedure);
end;

procedure TfrmFilter.ItemWithoutMorePageEnter(Sender: TObject);
begin
  ChangeToMorePage(mpNone);
end;

function TfrmFilter.GetTriStateCtrl(ALabel: TJvLabel): TTriState;
begin
  Result := ImageIndexToTriState(ALabel.ImageIndex);
end;

procedure TfrmFilter.SetTriStateCtrl(ALabel: TJvLabel;
  const AState: TTriState);
begin
  ALabel.ImageIndex := CImageIndexTriState[AState];
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

procedure TfrmFilter.GetDirectiveCtrls(var MustInclude, MustIncludeOneOf,
  MustExclude: TDirectives; const AArray: TDirectiveLabelArray);
var
  Directive: TDirective;
  FourState: TFourState;
begin
  MustInclude := [];
  MustIncludeOneOf := [];
  MustExclude := [];

  for Directive := Low(TDirective) to High(TDirective) do
    if Directive <> diOf then
    begin
      FourState := ImageIndexToFourState(AArray[Directive].ImageIndex);
      case FourState of
        fsNo: Include(MustExclude, Directive);
        fsOneOf: Include(MustIncludeOneOf, Directive);
        fsYes: Include(MustInclude, Directive);
      end;
    end;
end;

procedure TfrmFilter.GetScopeCtrls(var Scope: TClassVisibilities;
  const AArray: TScopeLabelArray);
var
  ClassVisibility: TClassVisibility;
begin
  Scope := [];
  for ClassVisibility := Low(TClassVisibility) to High(TClassVisibility) do
    if AArray[ClassVisibility].ImageIndex = CImageIndexBoolean[True] then
      Include(Scope, ClassVisibility);
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

procedure TfrmFilter.SetScopeCtrls(const Scope: TClassVisibilities;
  const AArray: TScopeLabelArray);
var
  ClassVisibility: TClassVisibility;
begin
  for ClassVisibility := Low(TClassVisibility) to High(TClassVisibility) do
    AArray[ClassVisibility].ImageIndex := CImageIndexBoolean[ClassVisibility in Scope];
end;

procedure TfrmFilter.BooleanLabelClick(Sender: TObject);
begin
  if Sender is TJvLabel then
    with Sender as TJvLabel do
      ImageIndex := CImageIndexBoolean[not ImageIndexToBoolean(ImageIndex)];
end;

procedure TfrmFilter.chbClassesEnter(Sender: TObject);
begin
  ChangeToMorePage(mpClass);
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

procedure TfrmFilter.actAddInExecute(Sender: TObject);
begin
  lsbPropertyIn.Items.Add(edtPropertyIn.Text);
end;

procedure TfrmFilter.actAddInUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := edtPropertyIn.Text <> '';
end;

end.

