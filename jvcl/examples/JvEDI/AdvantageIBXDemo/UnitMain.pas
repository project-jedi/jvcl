{**************************************************************************************************}
{                                                                                                  }
{ Ray's Jedi Projects                                                                              }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit is part of EDI SDK demos.                                                              }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: Before October, 1, 2003                                                            }
{ Last modified: April 2, 2004                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3@hotmail.com                                                           }
{   For latest EDI specific updates see http://sourceforge.net/projects/edisdk                     }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, adsdata, adsfunc, adstable, adscnnct, Menus, ComCtrls, Contnrs,
  JvEDIDBBuffering, JvComponent, StdCtrls, Grids, DBGrids, ExtCtrls,
  JclEDI, JclEDI_ANSIX12, JclEDISEF, adsdictionary;

type
  TFormMain = class(TForm)
    JvEDIDBSpecProfiler: TJvEDIDBSpecProfiler;
    pcMain: TPageControl;
    TabSheet1: TTabSheet;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    dbgL: TDBGrid;
    dbgS: TDBGrid;
    dbgE: TDBGrid;
    odOpenEDIFile: TOpenDialog;
    JvEDIDBSEFProfiler: TJvEDIDBSEFProfiler;
    TabSheet6: TTabSheet;
    rgDemoDatabaseOption: TRadioGroup;
    Button1: TButton;
    pb: TProgressBar;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    Button2: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure JvEDIDBSpecProfiler1AfterProfiledSegmentSpec(
      EDISegmentSpec: TEDISegmentSpec);
    procedure JvEDIDBSpecProfiler1AfterProfiledTransactionSetSpec(
      EDITransactionSetSpec: TEDITransactionSetSpec);
    procedure JvEDIDBSpecProfilerAfterProfiledSegment(
      Segment: TEDIObject);
    procedure JvEDIDBSEFProfilerAfterProfiledSegment(Segment: TEDIObject);
    procedure rgDemoDatabaseOptionClick(Sender: TObject);
  private
    { Private declarations }
    FEDIFileSpec: TEDIFileSpec;
    FEDISEFFile: TEDISEFFile;
  public
    { Public declarations }
  end;

const
  AdvantageDatabase = 0;
  Interbase6020_IBX = 1;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  ADS70_DM, Interbase6_IBX_DM;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ADS70_Data := TADS70_Data.Create(Self);
  ADS70_Data.RefreshProfileData;

  Interbase6_IBX_Data := TInterbase6_IBX_Data.Create(Self);
  FEDIFileSpec := TEDIFileSpec.Create(nil);
  FEDISEFFile := TEDISEFFile.Create(nil);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FEDISEFFile.Free;
  FEDIFileSpec.Free;

  Interbase6_IBX_Data.Free;
  ADS70_Data.Free;
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  ObjectList: TObjectList;
  I, J, K: Integer;
begin
  pb.Position := 0;
  pb.Min := 0;
  pb.Max := 0;

  case rgDemoDatabaseOption.ItemIndex of
    AdvantageDatabase:
    begin
      JvEDIDBSEFProfiler.ElementProfiles := ADS70_Data.EProfile;
      JvEDIDBSEFProfiler.SegmentProfiles := ADS70_Data.SProfile;
      JvEDIDBSEFProfiler.LoopProfiles := ADS70_Data.LProfile;
      dbgE.DataSource := ADS70_Data.dsEProfile;
      dbgS.DataSource := ADS70_Data.dsSProfile;
      dbgL.DataSource := ADS70_Data.dsLProfile;
      ADS70_Data.RefreshProfileData;
    end;
    Interbase6020_IBX:
    begin
      JvEDIDBSEFProfiler.ElementProfiles := Interbase6_IBX_Data.EProfile;
      JvEDIDBSEFProfiler.SegmentProfiles := Interbase6_IBX_Data.SProfile;
      JvEDIDBSEFProfiler.LoopProfiles := Interbase6_IBX_Data.LProfile;
      dbgE.DataSource := Interbase6_IBX_Data.dsEProfile;
      dbgS.DataSource := Interbase6_IBX_Data.dsSProfile;
      dbgL.DataSource := Interbase6_IBX_Data.dsLProfile;
      Interbase6_IBX_Data.RefreshProfileData;
    end;
  end; //case

  if odOpenEDIFile.Execute then
  begin
    if AnsiPos('.SEF', AnsiUpperCase(odOpenEDIFile.FileName)) > 0 then
    begin
      FEDISEFFile.LoadFromFile(odOpenEDIFile.FileName);
      FEDISEFFile.Disassemble;
      //Set progress bar max value
      for I := 0 to FEDISEFFile.SETS.Count - 1 do
      begin
        ObjectList := TEDISEFSet(FEDISEFFile.SETS[I]).GetSegmentObjectList;
        pb.Max := pb.Max + ObjectList.Count;
        ObjectList.Free;
      end;
      //
      JvEDIDBSEFProfiler.BuildProfile(FEDISEFFile);
    end
    else
    begin
      FEDIFileSpec.LoadFromFile(odOpenEDIFile.FileName);
      FEDIFileSpec.Disassemble;
      //Set progress bar max value
      for I := 0 to FEDIFileSpec.InterchangeControlCount - 1 do
        for J := 0 to FEDIFileSpec[I].FunctionalGroupCount - 1 do
          for K := 0 to FEDIFileSpec[I][J].TransactionSetCount - 1 do
            pb.Max := pb.Max + FEDIFileSpec[I][J][K].SegmentCount;
      //
      JvEDIDBSpecProfiler.BuildProfile(FEDIFileSpec);
    end;
    ShowMessage('Profile Done');
  end;

  case rgDemoDatabaseOption.ItemIndex of
    AdvantageDatabase:
    begin
      ADS70_Data.RefreshProfileData;
    end;
    Interbase6020_IBX:
    begin
      Interbase6_IBX_Data.RefreshProfileData;
    end;
  end; //case

end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;

  pb.Min := 0;
  pb.Position := 0;
  pb.Max := 0;

  case rgDemoDatabaseOption.ItemIndex of
    AdvantageDatabase:
    begin
      ADS70_Data.JvEDIDBBuffer.SyncProfilesWithBuffer;
    end;
    Interbase6020_IBX:
    begin
      Interbase6_IBX_Data.JvEDIDBBuffer.SyncProfilesWithBuffer;
      MessageDlg('The scripts generated in the output box need to be run in order '+#13+#10+
                 'to update the database buffer.', mtInformation, [mbOK], 0);
    end;
  end; //case

  
  ShowMessage('DB Buffer Update done!');
end;

procedure TFormMain.JvEDIDBSpecProfiler1AfterProfiledSegmentSpec(
  EDISegmentSpec: TEDISegmentSpec);
begin
  pb.StepIt;
end;

procedure TFormMain.JvEDIDBSpecProfiler1AfterProfiledTransactionSetSpec(
  EDITransactionSetSpec: TEDITransactionSetSpec);
begin
//
end;

procedure TFormMain.JvEDIDBSpecProfilerAfterProfiledSegment(
  Segment: TEDIObject);
begin
  pb.StepBy(1);
  Application.ProcessMessages;
end;

procedure TFormMain.JvEDIDBSEFProfilerAfterProfiledSegment(
  Segment: TEDIObject);
begin
  pb.StepBy(1);
  Application.ProcessMessages;  
end;

procedure TFormMain.rgDemoDatabaseOptionClick(Sender: TObject);
begin
  case rgDemoDatabaseOption.ItemIndex of
    AdvantageDatabase:
    begin
      JvEDIDBSEFProfiler.ElementProfiles := ADS70_Data.EProfile;
      JvEDIDBSEFProfiler.SegmentProfiles := ADS70_Data.SProfile;
      JvEDIDBSEFProfiler.LoopProfiles := ADS70_Data.LProfile;
      dbgE.DataSource := ADS70_Data.dsEProfile;
      dbgS.DataSource := ADS70_Data.dsSProfile;
      dbgL.DataSource := ADS70_Data.dsLProfile;
      ADS70_Data.RefreshProfileData;
    end;
    Interbase6020_IBX:
    begin
      JvEDIDBSEFProfiler.ElementProfiles := Interbase6_IBX_Data.EProfile;
      JvEDIDBSEFProfiler.SegmentProfiles := Interbase6_IBX_Data.SProfile;
      JvEDIDBSEFProfiler.LoopProfiles := Interbase6_IBX_Data.LProfile;
      dbgE.DataSource := Interbase6_IBX_Data.dsEProfile;
      dbgS.DataSource := Interbase6_IBX_Data.dsSProfile;
      dbgL.DataSource := Interbase6_IBX_Data.dsLProfile;
      Interbase6_IBX_Data.RefreshProfileData;
    end;
  end; //case
end;

end.
