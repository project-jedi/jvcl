{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}


{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit DBCbRest;

{$R-}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, BDE, JvLabel, JvComponent;

type
  TRestructureDialog = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    MessageGrp: TGroupBox;
    IconImage: TImage;
    YesBtn: TRadioButton;
    NoBtn: TRadioButton;
    ObjTypeLabel: TJvLabel ;
    ObjType: TEdit;
    ObjDescLabel: TJvLabel ;
    ObjDesc: TMemo;
    MsgLabel: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    FCBDesc: RESTCbDesc;
    function SetCBDesc(var RestInfo: RESTCbDesc): Boolean;
    function GetResult: CBRType;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

function RestCallback(var RestInfo: RESTCbDesc): CBRType;

var
  ShowRestWarnings: Boolean = True;

implementation

uses DB, DBTables, Consts, JvDBUtils, JvBdeUtils;

{$R *.DFM}
{$WARNINGS OFF}

function RestCallback(var RestInfo: RESTCbDesc): CBRType;
begin
  Result := cbrCONTINUE;
  with TRestructureDialog.Create(Application) do
  try
    if SetCBDesc(RestInfo) then begin
      if ShowModal = mrOK then Result := GetResult
      else Result := cbrABORT;
    end;
  finally
    Free;
  end;
end;

{ TRestructureDialog }

function TRestructureDialog.SetCBDesc(var RestInfo: RESTCbDesc): Boolean;
const
  SRIntType: array[RINTType] of PChar = ('Dependent', 'Master');
begin
  FCBDesc := RestInfo;
  MsgLabel.Lines.Add(BdeErrorMsg(FCBDesc.iErrCode));
  case FCBDesc.iErrCode of
    DBIERR_OBJMAYBETRUNCATED,
    DBIERR_TABLELEVELCHANGED,
    DBIERR_VALIDATEDATA:
      begin
        Result := True;
        YesBtn.Visible := True;
        NoBtn.Visible := True;
        YesBtn.Checked := True;
      end;
  else
    Result := ShowRestWarnings and
      ((FCBDesc.iErrCode = DBIERR_OBJIMPLICITLYMODIFIED) or
      (FCBDesc.iErrCode = DBIERR_OBJIMPLICITLYDROPPED) or
      (FCBDesc.iErrCode = DBIERR_VALFIELDMODIFIED) or
      (FCBDesc.iErrCode = DBIERR_VCHKMAYNOTBEENFORCED));
    YesBtn.Visible := False;
    NoBtn.Visible := False;
  end;
  if Result then begin
    ObjDesc.Lines.Clear;
    case FCBDesc.eRestrObjType of
      restrNEWFLD, restrOLDFLD:
        begin
          ObjType.Text := 'Data Field';
          ObjDesc.Lines.Add(StrPas(FCBDesc.uObjDesc.fldDesc.szName));
        end;
      restrNEWINDEX, restrOLDINDEX:
        begin
          ObjType.Text := 'Index';
          if FCBDesc.uObjDesc.idxDesc.szName[0] <> #0 then
            ObjDesc.Lines.Add(StrPas(FCBDesc.uObjDesc.idxDesc.szName));
          if FCBDesc.uObjDesc.idxDesc.szTagName[0] <> #0 then
            ObjDesc.Lines.Add(StrPas(FCBDesc.uObjDesc.idxDesc.szTagName));
          ObjDesc.Lines.Add(Format('Fields in the key: %d',
            [FCBDesc.uObjDesc.idxDesc.iFldsInKey]));
        end;
      restrNEWVCHK, restrOLDVCHK:
        begin
          ObjType.Text := 'Validity check';
          ObjDesc.Lines.Add(Format('Field number: %d',
            [FCBDesc.uObjDesc.vchkDesc.iFldNum]));
          if FCBDesc.uObjDesc.vchkDesc.bRequired then
            ObjDesc.Lines.Add('Value is required');
          if FCBDesc.uObjDesc.vchkDesc.bHasMinVal then
            ObjDesc.Lines.Add('Has min value');
          if FCBDesc.uObjDesc.vchkDesc.bHasMaxVal then
            ObjDesc.Lines.Add('Has max value');
          if FCBDesc.uObjDesc.vchkDesc.bHasDefVal then
            ObjDesc.Lines.Add('Has default value');
          if FCBDesc.uObjDesc.vchkDesc.szLkupTblName[0] <> #0 then
            ObjDesc.Lines.Add(Format('Lookup Table name: %d',
            [FCBDesc.uObjDesc.vchkDesc.szLkupTblName]));
        end;
      restrNEWRINT, restrOLDRINT:
        begin
          ObjType.Text := 'Referential integrity check';
          if FCBDesc.uObjDesc.rintDesc.szRintName[0] <> #0 then
            ObjDesc.Lines.Add(StrPas(FCBDesc.uObjDesc.rintDesc.szRintName));
          if FCBDesc.uObjDesc.rintDesc.szTblName[0] <> #0 then
            ObjDesc.Lines.Add(Format('%s table: %s',
            [SRIntType[FCBDesc.uObjDesc.rintDesc.eType],
            FCBDesc.uObjDesc.rintDesc.szTblName]));
        end;
      restrNEWSEC, restrOLDSEC:
        begin
          ObjType.Text := 'Auxilary password';
          if FCBDesc.uObjDesc.secDesc.eprvTable = prvREADONLY then
            ObjDesc.Lines.Add('Read Only')
          else if FCBDesc.uObjDesc.secDesc.eprvTable = prvMODIFY then
            ObjDesc.Lines.Add('Modify')
          else if FCBDesc.uObjDesc.secDesc.eprvTable = prvINSERT then
            ObjDesc.Lines.Add('Insert')
          else if FCBDesc.uObjDesc.secDesc.eprvTable = prvINSDEL then
            ObjDesc.Lines.Add('Insert / Delete')
          else if FCBDesc.uObjDesc.secDesc.eprvTable = prvFULL then
            ObjDesc.Lines.Add('Full Writes');
          if FCBDesc.uObjDesc.secDesc.szPassword[0] <> #0 then
            ObjDesc.Lines.Add(Format('Password: %s',
            [FCBDesc.uObjDesc.secDesc.szPassword]));
        end;
      restrNEWTABLE:
        begin
          ObjType.Text := 'New Table';
        end;
    end;
    ObjDescLabel.Enabled := ObjDesc.Lines.Count > 0;
  end;
end;

function TRestructureDialog.GetResult: CBRType;
begin
  if YesBtn.Visible then begin
    { DBIERR_OBJMAYBETRUNCATED, DBIERR_TABLELEVELCHANGED, DBIERR_VALIDATEDATA }
    if YesBtn.Checked then Result := cbrYES
    else if NoBtn.Checked then Result := cbrNO
    else Result := cbrUSEDEF;
  end
  else Result := cbrCONTINUE;
end;

procedure TRestructureDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

procedure TRestructureDialog.FormCreate(Sender: TObject);
begin
  Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
  IconImage.Picture.Icon := Icon;
end;

{ TRestructClass }

type
  TRestructClass = class(TObject)
  private
    FCBRest: RESTCbDesc;
    FRestrCallBack: TBDECallback;
    FCBBath: RESTCbDesc;
    FBatchCallBack: TBDECallback;
    function RestrCallBack(CBInfo: Pointer): CBRType;
    procedure RegisterCallbacks(Session: TSession);
  public
    destructor Destroy; override;
  end;

destructor TRestructClass.Destroy;
begin
  if Assigned(Session) and (Session.Active) then begin
    FBatchCallBack.Free;
    FRestrCallBack.Free;
  end;
  inherited Destroy;
end;

procedure TRestructClass.RegisterCallbacks(Session: TSession);
begin
  FRestrCallBack := TBDECallback.Create(Self, nil, cbRESTRUCTURE,
    @FCBRest, SizeOf(FCBRest), RestrCallBack, False);
  FBatchCallBack := TBDECallback.Create(Self, nil, cbBATCHRESULT,
    @FCBBath, SizeOf(FCBBath), RestrCallBack, False);
end;

function TRestructClass.RestrCallBack(CBInfo: Pointer): CBRType;
begin
  try
    Result := RestCallback(RESTCbDesc(CBInfo^));
  except
    Result := cbrUseDef;
  end;
end;

var
  RestructClass: TRestructClass;

procedure InitProc(Session: TSession);
begin
  RestructClass.RegisterCallbacks(Session);
end;

initialization
  RestructClass := TRestructClass.Create;
  RegisterBDEInitProc(InitProc);
finalization
  RestructClass.Free;
end.
