{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit BdeProp;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DB, JvxCtrls, BdeInfo;

type
  TBdePropertyDlg = class(TForm)
    GroupBox1: TGroupBox;
    Bevel3: TBevel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    FS: TLabel;
    FD: TLabel;
    FT: TLabel;
    FSL: TLabel;
    FDL: TLabel;
    FTL: TLabel;
    BDEDLLList: TJvTextListBox ;
    Button1: TButton;
    GroupBox2: TGroupBox;
    SysInformation: TJvTextListBox ;
    procedure DLLListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FBdeInfo: TBdeInfo;
    procedure UpdateFiles(const FileName: string);
    procedure UpdateInformation;
  public
    { Public declarations }
  end;

implementation

uses JvFileUtil, JvVCLUtils;

{$R *.DFM}

procedure TBdePropertyDlg.UpdateInformation;
var
  DT: TDateTime;
begin
  with FBdeInfo do begin
    DT := VersionDateTime;
    with SysInformation.Items do begin
      Clear;
      Add(Format('CFG File Path: %s%s', [Char(VK_TAB), ConfigPath]));
      Add(Format('Engine Version: %s%.2f', [Char(VK_TAB),
        EngineVersion / 100]));
      Add(Format('Interface Level: %s%.2f', [Char(VK_TAB),
        InterfaceLevel / 100]));
      Add(Format('Version Date: %s%s', [Char(VK_TAB), DateToStr(DT)]));
      Add(Format('Version Time: %s%s', [Char(VK_TAB),
        FormatDateTime('hh:mm', DT)]));
      if NetworkType <> '' then
        Add(Format('Network Type: %s%s', [Char(VK_TAB), NetworkType]));
      if NetUserName <> '' then
        Add(Format('Net User Name: %s%s', [Char(VK_TAB), NetUserName]));
      StartWait;
      try
        Add(Format('Language Driver: %s%s', [Char(VK_TAB), LangDriverDesc]));
      finally
        StopWait;
      end;
      Add(Format('Active Drivers: %s%d', [Char(VK_TAB), ActiveDrivers]));
      Add(Format('Active Clients: %s%d', [Char(VK_TAB), ActiveClients]));
      Add(Format('Active Sessions: %s%d', [Char(VK_TAB), ActiveSessions]));
      Add(Format('Active Databases: %s%d', [Char(VK_TAB), ActiveDatabases]));
      Add(Format('Active Cursors: %s%d', [Char(VK_TAB), ActiveCursors]));
      Add(Format('Buffer Size: %s%d K', [Char(VK_TAB), BufferSpace]));
      Add(Format('Heap Size: %s%d K', [Char(VK_TAB), HeapSpace]));
    end;
  end;
  Self.BDEDLLList.Items.Assign(FBdeInfo.BdeDllList);
  if BDEDLLList.Items.Count > 0 then BDEDLLList.ItemIndex := 0;
  DLLListClick(nil);
end;

procedure TBdePropertyDlg.UpdateFiles(const FileName: string);
var
  Rec: TSearchRec;
  DT: TDateTime;
begin
  FTL.Caption := '';
  FDL.Caption := '';
  FSL.Caption := '';
  if (FileName <> '') and (FindFirst(FileName, faAnyFile, Rec) = 0) then begin
    DT := FileDateToDateTime(Rec.Time);
    FTL.Caption := TimeToStr(DT);
    FDL.Caption := DateToStr(DT);
    FSL.Caption := IntToStr(Rec.Size);
    FindClose(Rec);
  end;
end;

procedure TBdePropertyDlg.DLLListClick(Sender: TObject);
begin
  if BDEDLLList.ItemIndex >= 0 then
    UpdateFiles(NormalDir(FBdeInfo.BdeDirectory) +
      BDEDLLList.Items[BDEDLLList.ItemIndex])
  else UpdateFiles('');
end;

procedure TBdePropertyDlg.FormCreate(Sender: TObject);
begin
  FBdeInfo := TBdeInfo.Create;
  UpdateInformation;
end;

procedure TBdePropertyDlg.FormDestroy(Sender: TObject);
begin
  FBdeInfo.Free;
end;

end.
