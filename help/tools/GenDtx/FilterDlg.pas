unit FilterDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList,

  ParserTypes, ExtCtrls;

type
  TDuplicatesType = (dtHide, dtHideCaseSensitive, dtOnlyDuplicates,
    dtOnlyCaseSensitiveDuplicates, dtAll);

  TFilterData = record
    RShow: TDelphiTypes;
    RDuplicates: TDuplicatesType;
  end;

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
    Bevel1: TBevel;
    Bevel2: TBevel;
    rgrDuplicatesOrUnique: TRadioGroup;
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
  private
    FFilter: TFilterData;
  protected
    procedure CtrlsToData;
    procedure DataToCtrls;
  public
    class function Execute(var AFilter: TFilterData): Boolean;
  end;

implementation

{$R *.dfm}

procedure TfrmFilter.actOKExecute(Sender: TObject);
begin
  CtrlsToData;
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
    FFilter := AFilter;
    DataToCtrls;
    Result := ShowModal = mrOk;
    if Result then
      AFilter := FFilter;
  finally
    Free;
  end;
end;

procedure TfrmFilter.CtrlsToData;
begin
  FFilter.RShow := [];
  if chbClasses.Checked then
    Include(FFilter.RShow, dtClass);
  if chbConst.Checked then
    Include(FFilter.RShow, dtConst);
  if chbDispInterface.Checked then
    Include(FFilter.RShow, dtDispInterface);
  if chbFunction.Checked then
    Include(FFilter.RShow, dtFunction);
  if chbFunctionType.Checked then
    Include(FFilter.RShow, dtFunctionType);
  if chbInterface.Checked then
    Include(FFilter.RShow, dtInterface);
  if chbMethodFunc.Checked then
    Include(FFilter.RShow, dtMethodFunc);
  if chbMethodProc.Checked then
    Include(FFilter.RShow, dtMethodProc);
  if chbProcedure.Checked then
    Include(FFilter.RShow, dtProcedure);
  if chbProcedureType.Checked then
    Include(FFilter.RShow, dtProcedureType);
  if chbProperty.Checked then
    Include(FFilter.RShow, dtProperty);
  if chbRecord.Checked then
    Include(FFilter.RShow, dtRecord);
  if chbResourcestring.Checked then
    Include(FFilter.RShow, dtResourcestring);
  if chbEnum.Checked then
    Include(FFilter.RShow, dtEnum);
  if chbType.Checked then
    Include(FFilter.RShow, dtType);
  if chbVar.Checked then
    Include(FFilter.RShow, dtVar);

  FFilter.RDuplicates := TDuplicatesType(rgrDuplicatesOrUnique.ItemIndex);
end;

procedure TfrmFilter.DataToCtrls;
begin
  chbClasses.Checked := dtClass in FFilter.RShow;
  chbConst.Checked := dtConst in FFilter.RShow;
  chbDispInterface.Checked := dtDispInterface in FFilter.RShow;
  chbFunction.Checked := dtFunction in FFilter.RShow;
  chbFunctionType.Checked := dtFunctionType in FFilter.RShow;
  chbInterface.Checked := dtInterface in FFilter.RShow;
  chbMethodFunc.Checked := dtMethodFunc in FFilter.RShow;
  chbMethodProc.Checked := dtMethodProc in FFilter.RShow;
  chbProcedure.Checked := dtProcedure in FFilter.RShow;
  chbProcedureType.Checked := dtProcedureType in FFilter.RShow;
  chbProperty.Checked := dtProperty in FFilter.RShow;
  chbRecord.Checked := dtRecord in FFilter.RShow;
  chbResourcestring.Checked := dtResourcestring in FFilter.RShow;
  chbEnum.Checked := dtEnum in FFilter.RShow;
  chbType.Checked := dtType in FFilter.RShow;
  chbVar.Checked := dtVar in FFilter.RShow;

  rgrDuplicatesOrUnique.ItemIndex := Integer(FFilter.RDuplicates);
end;

end.

