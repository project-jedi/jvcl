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
  DBExplorerDemoMainForm in 'DBExplorerDemoMainForm.PAS' {DBExplorerMainForm},
  CHILDWIN2 in 'CHILDWIN2.PAS' {MDIChild},
  OPENDLG in 'OPENDLG.PAS' {OpenDatabaseDlg},
  DESTTAB in 'DESTTAB.PAS' {DestTableDlg},
  ABOUT in 'ABOUT.PAS' {AboutDlg},
  EDITPICT in 'EDITPICT.PAS' {PictEditDlg},
  EDITSTR in 'EDITSTR.PAS' {StrEditDlg},
  VIEWBLOB in 'VIEWBLOB.PAS' {BlobViewDlg},
  FILTDLG in 'FILTDLG.PAS' {FilterDialog},
  OPTDLG in 'OPTDLG.PAS' {OptionsDialog},
  SRCTAB in 'SRCTAB.PAS' {SrcTableDlg},
  SQLMON in 'SQLMON.PAS' {TraceSQL},
  OPTIONS in 'OPTIONS.PAS',
  RENDLG in 'RENDLG.PAS' {RenameDialog},
  BDEINFO in 'BDEINFO.PAS',
  BDEPROP in 'BDEPROP.PAS' {BdePropertyDlg};

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
