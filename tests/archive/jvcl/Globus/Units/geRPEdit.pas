unit geRPEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Mask;

type
  TReportParamEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    rgParameterType: TRadioGroup;
    Notebook: TNotebook;
    gbTextMask: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    meTestMask: TMaskEdit;
    eTextMask: TEdit;
    gbRadioItems: TGroupBox;
    Label5: TLabel;
    lbRadioItems: TListBox;
    pbAddItem: TButton;
    pbDeleteItem: TButton;
    eItemToAdd: TEdit;
    pbInsertItem: TButton;
    gbCheckbox: TGroupBox;
    Label7: TLabel;
    eCheckBox: TEdit;
    gbDataSource: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    eTableName: TEdit;
    eFieldName: TEdit;
    procedure rgParameterTypeClick(Sender: TObject);
    procedure pbAddItemClick(Sender: TObject);
    procedure lbRadioItemsClick(Sender: TObject);
    procedure eTextMaskChange(Sender: TObject);
    procedure pbDeleteItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReportParamEditor: TReportParamEditor;

implementation

{$R *.DFM}

{procedure SetEnabledState( WC: TWinControl; State: boolean );
var i: integer;
begin
  for i:=0 to WC.ControlCount-1 do
    WC.Controls[i].Enabled := State;
end;}

procedure TReportParamEditor.rgParameterTypeClick(Sender: TObject);
begin
  Notebook.PageIndex := rgParameterType.ItemIndex;
end;

procedure TReportParamEditor.pbAddItemClick(Sender: TObject);
begin
  eItemToAdd.Text := Trim(eItemToAdd.Text);
  if length(eItemToAdd.Text) > 0 then
    if TButton(Sender).Tag = 0 then lbRadioItems.Items.Append(eItemToAdd.Text)
    else if lbRadioItems.ItemIndex <> -1 then
          lbRadioItems.Items.Insert(lbRadioItems.ItemIndex, eItemToAdd.Text);
end;

procedure TReportParamEditor.lbRadioItemsClick(Sender: TObject);
begin
  pbDeleteItem.Enabled := lbRadioItems.ItemIndex <> -1;
  pbInsertItem.Enabled := pbDeleteItem.Enabled;
end;

procedure TReportParamEditor.eTextMaskChange(Sender: TObject);
begin
  meTestMask.EditMask := eTextMask.Text;
end;

procedure TReportParamEditor.pbDeleteItemClick(Sender: TObject);
begin
  lbRadioItems.Items.Delete(lbRadioItems.ItemIndex);
end;

end.
