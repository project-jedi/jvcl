{$I JVCL.INC}
unit JvValidatorsEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, JvValidators, DesignEditors, DesignIntf, DesignWindows,
  StdCtrls, Menus, ActnList, ImgList;

type
  TfrmValidatorsEditor = class(TDesignWindow)
    ToolBar1: TToolBar;
    btnNew: TToolButton;
    btnDelete: TToolButton;
    StatusBar1: TStatusBar;
    lbValidators: TListBox;
    popNew: TPopupMenu;
    RequiredFieldValidator1: TMenuItem;
    RangeValidator1: TMenuItem;
    RegularExpressionValidator1: TMenuItem;
    CompareValidator1: TMenuItem;
    CustomValidator1: TMenuItem;
    alEditor: TActionList;
    acNewRequired: TAction;
    acNewCompare: TAction;
    acNewRange: TAction;
    acNewRegExp: TAction;
    acNewCustom: TAction;
    acDelete: TAction;
    ImageList1: TImageList;
    procedure alEditorUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acNewRequiredExecute(Sender: TObject);
    procedure acNewCompareExecute(Sender: TObject);
    procedure acNewRangeExecute(Sender: TObject);
    procedure acNewRegExpExecute(Sender: TObject);
    procedure acNewCustomExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure lbValidatorsClick(Sender: TObject);
  private
    { Private declarations }
    FValidators:TJvValidatorList;
    FValidator:TJvValidator;
    function AddValidator(Validator:TJvBaseValidator):integer;
    procedure DeleteValidator(Index:integer);
    procedure ClearValidators;
    procedure SelectValidator(AObject:TPersistent);
  public
    {$IFDEF COMPILER6_UP}
    procedure ItemInserted(const ADesigner: IDesigner; Item: TPersistent);override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    {$ELSE}
    procedure ComponentDeleted(Component: IPersistent); override;
    function UniqueName(Component: TComponent): string; override;
    procedure FormClosed(AForm: TCustomForm); override;
    procedure FormModified; override;
    {$ENDIF}
    function GetEditState: TEditState; override;
  end;

procedure ShowEditor(Designer:IDesigner;AValidator:TJvValidator; AValidators:TJvValidatorList);

implementation

var
  frm:TfrmValidatorsEditor = nil;

{$R *.dfm}
procedure ShowEditor(Designer:IDesigner;AValidator:TJvValidator; AValidators:TJvValidatorList);
var i:integer;
begin
  if frm = nil then
    frm := TfrmValidatorsEditor.Create(Application);
  frm.Designer := Designer;
  frm.FValidators := AValidators;
  frm.FValidator  := AValidator;
  frm.ClearValidators;
  if AValidators <> nil then
    for i := 0 to AValidators.Count - 1 do
      frm.AddValidator(AValidators[i]);
  frm.Show;
end;

function TfrmValidatorsEditor.AddValidator(Validator: TJvBaseValidator): integer;
begin
  Result := lbValidators.Items.AddObject(Validator.Name,Validator);
  lbValidators.ItemIndex := Result;
end;

procedure TfrmValidatorsEditor.alEditorUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if (FValidator = nil) or (FValidators = nil) then
    alEditor.State := asSuspended
  else
    alEditor.State := asNormal;
  acDelete.Enabled := lbValidators.ItemIndex > -1;
end;

procedure TfrmValidatorsEditor.ClearValidators;
begin
  lbValidators.Items.Clear;
end;

{$IFDEF COMPILER6_UP}

procedure TfrmValidatorsEditor.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TfrmValidatorsEditor.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
var i,j:integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    j := lbValidators.ItemIndex;
    ClearValidators;
    for i := 0 to FValidators.Count - 1 do
      AddValidator(FValidators[i]);
    lbValidators.ItemIndex := j;
  end;
end;

procedure TfrmValidatorsEditor.ItemsModified(const Designer: IDesigner);
var i,j:integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    j := lbValidators.ItemIndex;
    ClearValidators;
    for i := 0 to FValidators.Count - 1 do
      AddValidator(FValidators[i]);
    lbValidators.ItemIndex := j;
  end;
end;
{$ELSE }
procedure TfrmValidatorsEditor.ComponentDeleted(Component: IPersistent);
begin
  if Component = FValidator then
  begin
    FValidator = nil;
    FValidators := nil;
    ClearValidators;
  end;

end;

procedure TfrmValidatorsEditor.FormClosed(AForm: TCustomForm);
begin
  if AForm = Designer.Form then
    Close;
end;

procedure TfrmValidatorsEditor.FormModified;
begin
  inherited;

end;

function TfrmValidatorsEditor.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;

{$ENDIF}

procedure TfrmValidatorsEditor.DeleteValidator(Index: integer);
begin
  if (Index > -1) and (Index < lbValidators.Items.Count) then
    lbValidators.Items.Delete(Index);
end;


function TfrmValidatorsEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TfrmValidatorsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frm := nil;
  Action := caFree;
end;

procedure TfrmValidatorsEditor.acNewRequiredExecute(Sender: TObject);
var i:integer;
begin
  i := FValidators.Add(TJvRequiredFieldValidator);
  AddValidator(FValidators[i]);
end;

procedure TfrmValidatorsEditor.acNewCompareExecute(Sender: TObject);
var i:integer;
begin
  i := FValidators.Add(TJvCompareValidator);
  AddValidator(FValidators[i]);
end;

procedure TfrmValidatorsEditor.acNewRangeExecute(Sender: TObject);
var i:integer;
begin
  i := FValidators.Add(TJvRangeValidator);
  AddValidator(FValidators[i]);
end;

procedure TfrmValidatorsEditor.acNewRegExpExecute(Sender: TObject);
var i:integer;
begin
  i := FValidators.Add(TJvRegularExpressionValidator);
  AddValidator(FValidators[i]);
end;

procedure TfrmValidatorsEditor.acNewCustomExecute(Sender: TObject);
var i:integer;
begin
  i := FValidators.Add(TJvCustomValidator);
  AddValidator(FValidators[i]);
end;

procedure TfrmValidatorsEditor.acDeleteExecute(Sender: TObject);
begin
  FValidators.Delete(lbValidators.ItemIndex);
  DeleteValidator(lbValidators.ItemIndex);
end;

procedure TfrmValidatorsEditor.lbValidatorsClick(Sender: TObject);
begin
  if lbValidators.ItemIndex > -1 then
  with lbValidators do
    SelectValidator(TJvBaseValidator(Items.Objects[ItemIndex]));
end;

procedure TfrmValidatorsEditor.SelectValidator(AObject: TPersistent);
begin
  Designer.SelectComponent(AObject);
  Designer.Modified;
end;

procedure TfrmValidatorsEditor.ItemInserted(const ADesigner: IDesigner;
  Item: TPersistent);
begin
  if Item is TJvBaseValidator then
    TJvBaseValidator(Item).Name := ADesigner.UniqueName(Item.ClassName)
  else
    inherited;
end;

end.
