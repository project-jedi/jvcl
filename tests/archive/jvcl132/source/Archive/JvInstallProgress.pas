{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInstallProgress.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvInstallProgress;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Jpeg, JvFormInstall, JvInstallerPage;

type
  TJvInstallProgress = class;
  TJvInstallProgressOptions = class;

  TJvInstallProgression = class(TPersistent)
  private
    FCurrent: Integer;
    Fmaximum: Integer;
    FOnChange: TNotifyEvent;
    FOwner: TJvInstallProgressOptions;
    FStartTime: TTime;
    FLastEstimationValue: TTime;
    procedure SeCurrent(const Value: integer);
    procedure SetMaximum(const Value: integer);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property Owner:TJvInstallProgressOptions read FOwner write FOwner;
    procedure StartStat;
    function GetTimeLeft:TTime;
  public
    constructor Create;
  published
    property Maximum:Integer read Fmaximum write SetMaximum default 100;
    property Progress:Integer read FCurrent write SeCurrent default 0;
  end;

  TJvInstallProgressOptions = class(TPersistent)
  private
    Ftext: string;
    FFileName: string;
    FCurrent: TJvInstallProgression;
    FTotal: TJvInstallProgression;
    FOnChange: TNotifyEvent;
    FOwner: TJvInstallProgress;
    procedure SetFileName(const Value: string);
    procedure SetText(const Value: string);
    function LimitTextDir(Value: string; Canvas: TCanvas;
      MaxWidth: integer): string;
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    procedure OptionsChanged(Sender: TObject);
    property Owner:TJvInstallProgress read FOwner write FOwner;
    procedure StartCounter;
  public
    constructor Create;
    destructor Destroy;override;
  published
    property Text:string read Ftext write SetText;
    property FileName:string read FFileName write SetFileName;
    property Current:TJvInstallProgression read FCurrent write FCurrent;
    property Total:TJvInstallProgression read FTotal write FTotal;
  end;

  TJvInstallProgress = class(TJvInstallerPage)
  private
    FForm:TFormInst;
    FOptions: TJvInstallProgressOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    procedure Terminate;
    property Options:TJvInstallProgressOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';
   RC_Copy1                 =       'The program is now copying files for ';
   RC_Copy2                 =       '. Please wait...';


{***********************************************}
constructor TJvInstallProgress.Create(AOwner: TComponent);
begin
  inherited;
  FForm:=TFormInst.Create(Application);
  self.Buttons.Previous.Enabled:=false;
  self.Buttons.Next.Enabled:=false;

  FOptions:=TJvInstallProgressOptions.Create;
  FOptions.OnChange:=OptionsChanged;
  FOptions.Owner:=self;
end;
{***********************************************}
destructor TJvInstallProgress.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvInstallProgress.Execute;
begin
  inherited;
  Application.ProcessMessages;
  if Installer<>nil then
  begin
    FOptions.StartCounter;

    //assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FForm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    OptionsChanged(nil);

    if (ImageNotEmpty) then
       FForm.Image1.picture.bitmap.assign(Installer.Options.picture);

    FForm.tag:=0;
    FForm.Formstyle:=fsStayOnTop;
    FForm.Visible:=true;
  end
  else
      raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvInstallProgress.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvInstallProgress.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    if Options.text<>'' then Fform.StaticText1.caption:=Options.Text
    else FForm.Statictext1.Caption:=RC_Copy1+Installer.Options.ProgramName+RC_Copy2;

    FForm.Gauge1.Max:=FOptions.Current.Maximum;
    FForm.Gauge1.Position:=FOptions.Current.Progress;

    FForm.Gauge2.Max:=FOptions.Total.Maximum;
    FForm.Gauge2.Position:=FOptions.Total.Progress;

    Application.ProcessMessages;
  end;
end;
{***********************************************}
procedure TJvInstallProgress.Terminate;
begin
  self.Buttons.Next.Enabled:=true;
  FForm.BuButton2.Click;
end;
{***********************************************}
procedure TJvInstallProgress.UpdateButtons;
begin
  inherited;
  if FForm<>nil then
  begin
    Buttons.Previous.AssignTo(FForm.BuButton1);
    Buttons.Next.AssignTo(FForm.BuButton2);
    Buttons.Cancel.AssignTo(FForm.BuButton3);
  end;
end;
{***********************************************}


{ TJvInstallProgression }


{***********************************************}
constructor TJvInstallProgression.Create;
begin
  FMaximum := 100;
  FCurrent := 0;
  FLastEstimationValue := -1;
end;
{***********************************************}
function TJvInstallProgression.GetTimeLeft: TTime;
var
  d:double;
begin
  if FCurrent<>0 then
  begin
    try
      d := Time-FStartTime;
      result:=(d/Progress*Maximum)-(d);
    except
      result:=0.0;
    end;
  end
  else
     result:=0.0;
  if (result>FLastEstimationValue) and (result-FLastEstimationValue<EncodeTime(0,0,5,0)) then
    result := FLastEstimationValue
  else
    FLastEstimationValue := result
end;
{***********************************************}
procedure TJvInstallProgression.SeCurrent(const Value: integer);
begin
  FCurrent := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallProgression.SetMaximum(const Value: integer);
begin
  FMaximum := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallProgression.StartStat;
begin
  FStartTime := Time;
  FLastEstimationValue := -1;
end;
{***********************************************}


{ TJvInstallProgressOptions }


{***********************************************}
constructor TJvInstallProgressOptions.Create;
begin
  FCurrent:=TJvInstallProgression.Create;
  FCurrent.OnChange:=OptionsChanged;
  FCurrent.Owner:=self;

  FTotal:=TJvInstallProgression.Create;
  FTotal.OnChange:=OptionsChanged;
  FTotal.Owner:=self;
end;
{***********************************************}
destructor TJvInstallProgressOptions.Destroy;
begin
  FCurrent.Free;
  FTotal.Free;
  inherited;
end;
{************************************************************}
function TJvInstallProgressOptions.LimitTextDir(Value:string;Canvas:TCanvas;MaxWidth:integer):string;
var
  i:Integer;
  st,st2,st3:string;

  function ExtractLastDir(var Value:string):string;
  var
   i:Integer;
  begin
    if trim(Value)='' then result:=''
    else
    begin
      if Value[length(Value)]='\' then Value:=Copy(Value,1,length(Value)-1);
      i:=length(Value);
      while (i>0) and (Value[i]<>'\') do dec(i);
      if i=0 then
      begin
        result:=Value;
        Value:='';
      end
      else
      begin
        result:=Copy(Value,i+1,length(Value));
        Value:=Copy(Value,1,i-1);
      end;
    end;
  end;

begin
  i:=Canvas.TextWidth(Value);
  if i>MaxWidth then
  begin
    st:=ExtractFileName(Value);
    Value:=ExtractFilePath(Value);
    st2:=Copy(Value,1,pos('\',Value));
    st3:=st2+'..\'+st;
    while (Canvas.TextWidth(st+st2+'..\')<MaxWidth) do
    begin
      st3:=st2+'..\'+st;
      st:=ExtractLastDir(Value)+'\'+st;
    end;
    Value:=st3;
  end;
  result:=Value;
end;
{***********************************************}
procedure TJvInstallProgressOptions.OptionsChanged(Sender: TObject);
begin
  Owner.FForm.CurrentLeft.Caption := FormatDateTime('hh:nn:ss',Current.GetTimeLeft);
  Owner.FForm.TotalLeft.Caption := FormatDateTime('hh:nn:ss',Total.GetTimeLeft);
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallProgressOptions.SetFileName(const Value: string);
begin
  FFileName := Value;
  if (Owner<>nil) and (Owner.FForm<>nil) then
  begin
    Owner.FForm.FileName.Caption:=LimitTextDir(Value,Owner.FForm.FileName.Canvas,
         Owner.FForm.FileName.Width);
    Owner.FForm.CurrentLeft.caption:='';
  end;
end;
{***********************************************}
procedure TJvInstallProgressOptions.SetText(const Value: string);
begin
  Ftext := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallProgressOptions.StartCounter;
begin
  FCurrent.StartStat;
  FTotal.StartStat;
end;
{***********************************************}

end.
