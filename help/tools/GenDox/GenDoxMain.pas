unit GenDoxMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, StdCtrls, ExtCtrls,
  JvFormPlacement, JvAppStorage, JvAppRegistryStorage;

type
  TfrmMain = class(TForm)
    pnlSettings: TPanel;
    lblJVCLDir: TLabel;
    edJVCLBaseFolder: TEdit;
    btnBrowse: TButton;
    cxIncludeLocked: TCheckBox;
    cxGenFullHelp: TCheckBox;
    cxGenPackagesHelp: TCheckBox;
    pnlProgress: TPanel;
    pnlAction: TPanel;
    lblActionCaption: TLabel;
    lblAction: TLabel;
    pnlActionInfo: TPanel;
    lblActionInfoCaption: TLabel;
    lblActionInfo: TLabel;
    btnGenerate: TButton;
    btnClose: TButton;
    StorageLinks: TJvFormStorage;
    JvAppRegistryStorage: TJvAppRegistryStorage;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateInform(Action, SubActionCaption, SubAction: string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  GenDoxEngine;
  
{$R *.DFM}

procedure TfrmMain.UpdateInform(Action, SubActionCaption, SubAction: string);
begin
  lblAction.Caption := Action;
  lblActionInfoCaption.Caption := SubActionCaption;
  lblActionInfo.Caption := SubAction;
  Application.ProcessMessages;
end;

procedure TfrmMain.btnBrowseClick(Sender: TObject);
begin
//
end;

procedure TfrmMain.btnGenerateClick(Sender: TObject);
var
  PackageList: TDtxPackages;
  DtxList: TDtxInfos;
begin
  GatherInfo(edJVCLBaseFolder.Text + '\run', UpdateInform, PackageList, DtxList);
  try
    Analyze(UpdateInform, PackageList, DtxList, cxIncludeLocked.Checked);
    if cxGenFullHelp.Checked then
      BuildMainHelpDOX(UpdateInform, DtxList);
//    if cxGenPackagesHelp.Checked then
//      BuildPackagesHelpDOX(UpdateInform, edJVCLBaseFolder.Text + '\run', PackageList);
  finally
    // NOTE: !!!do not switch the order of destruction for DtxList and PackageList!!!
    DtxList.Free;
    PackageList.Free;
    lblAction.Caption := 'Done.';
    lblActionInfoCaption.Caption := '';
    lblActionInfo.Caption := '';
  end;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
