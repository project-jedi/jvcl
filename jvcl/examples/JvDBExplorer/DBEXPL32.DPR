{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

program DbExpl32;

uses
  Forms,
  JvBDEExceptionForm,
  JvJVCLUtils,
  Main in 'MAIN.PAS' {DBExplorerMainForm},
  ChildWin2 in 'CHILDWIN2.PAS' {MDIChild},
  OpenDlg in 'OPENDLG.PAS' {OpenDatabaseDlg},
  DestTab in 'DESTTAB.PAS' {DestTableDlg},
  About in 'ABOUT.PAS' {AboutDlg},
  EditPict in 'EDITPICT.PAS' {PictEditDlg},
  EditStr in 'EDITSTR.PAS' {StrEditDlg},
  ViewBlob in 'VIEWBLOB.PAS' {BlobViewDlg},
  FiltDlg in 'FILTDLG.PAS' {FilterDialog},
  OptDlg in 'OPTDLG.PAS' {OptionsDialog},
  SrcTab in 'SRCTAB.PAS' {SrcTableDlg},
  SqlMon in 'SQLMON.PAS' {TraceSQL},
  Options in 'OPTIONS.PAS',
  RenDlg in 'RENDLG.PAS' {RenameDialog},
  BdeInfo in 'BDEINFO.PAS',
  BdeProp in 'BDEPROP.PAS' {BdePropertyDlg};

{$R *.RES}

begin
  Application.Initialize;
  { Uncomment next line to allow start only one instance of DBEXPL32.EXE  }
  { if ActivatePrevInstance(TDBExplorerMainForm.ClassName, '') then Exit; }
  Application.Title := 'Database Explorer';
  DBErrorIntercept;
  Application.CreateForm(TDBExplorerMainForm, DBExplorerMainForm);
  Application.Run;
end.
