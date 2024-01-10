{$I JVCL.INC}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvObjPickerComp, JvComponent;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ActiveX, JvObjSel, ComObj;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------
// This is the code used in the article (only slightly different)
//----------------------------------------------------------------------------------------------------------------------

// just a convenience constant, includes all security groups

const
  ALL_SECURITY_GROUPS =
    DSOP_FILTER_DOMAIN_LOCAL_GROUPS_SE or DSOP_FILTER_BUILTIN_GROUPS or
    DSOP_FILTER_GLOBAL_GROUPS_SE or DSOP_FILTER_UNIVERSAL_GROUPS_SE;

// TOleVariantArray is just a convenience to access the pvarFetchedAttributes as an array

type
  TOleVariantArray = array [0..(MaxInt div SizeOf(OleVariant)) - 1] of OleVariant;
  POleVariantArray = ^TOleVariantArray;

procedure TForm1.Button2Click(Sender: TObject);
var
  ObjPicker: IDsObjectPicker;             // the Object Picker COM object
  InitInfo: TDsOpInitInfo;                // the initalization record
  ScopeInitInfo: array [0..1] of TDsOpScopeInitInfo; // the scopes
  DataObj: IDataObject;                   // used to retrieve the selection list
  Format: TFormatEtc;                     // used to retrieve the selection list
  Medium: TStgMedium;                     // used to retrieve the selection list
  SelectionList: PDsSelectionList;        // the list of selected objects
  Selection: TDsSelection;                // selection object
  HR: HResult;                            // method call result
  I: Integer;                             // loop variable
  Attributes: array [0..1] of WideString; // array of requested attribute names
  Attribute: OleVariant;                  // value of requested attribute
begin
  // 1. initialize COM
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  // 2. create the Object Picker object
  if SUCCEEDED(CoCreateInstance(CLSID_DsObjectPicker, nil, CLSCTX_INPROC_SERVER, IID_IDsObjectPicker, ObjPicker)) then
  begin
    // 3. initialize scopes
    FillChar(InitInfo, SizeOf(InitInfo), 0);
    InitInfo.cbSize := SizeOf(InitInfo);
    InitInfo.flOptions := DSOP_FLAG_MULTISELECT;
    InitInfo.cDsScopeInfos := 2;
    InitInfo.aDsScopeInfos := @ScopeInitInfo[0];
    // 1st scope, users from enterprise domain
    FillChar(ScopeInitInfo[0], SizeOf(TDsOpScopeInitInfo), 0);
    ScopeInitInfo[0].cbSize := SizeOf(TDsOpScopeInitInfo);
    ScopeInitInfo[0].flType := DSOP_SCOPE_TYPE_ENTERPRISE_DOMAIN;
    ScopeInitInfo[0].FilterFlags.Uplevel.flMixedModeOnly := DSOP_FILTER_USERS;
    ScopeInitInfo[0].FilterFlags.Uplevel.flBothModes := DSOP_FILTER_USERS;
    ScopeInitInfo[0].FilterFlags.Uplevel.flNativeModeOnly := DSOP_FILTER_USERS;
    // 2nd scope, security accounts from global catalog
    FillChar(ScopeInitInfo[1], SizeOf(TDsOpScopeInitInfo), 0);
    ScopeInitInfo[1].cbSize := SizeOf(TDsOpScopeInitInfo);
    ScopeInitInfo[1].flType := DSOP_SCOPE_TYPE_GLOBAL_CATALOG;
    ScopeInitInfo[1].FilterFlags.Uplevel.flMixedModeOnly := ALL_SECURITY_GROUPS;
    ScopeInitInfo[1].FilterFlags.Uplevel.flBothModes := ALL_SECURITY_GROUPS;
    ScopeInitInfo[1].FilterFlags.Uplevel.flNativeModeOnly := ALL_SECURITY_GROUPS;
    // 4. initialize attributes, request displayname and email address
    Attributes[0] := WideString('displayName');
    Attributes[1] := WideString('mail');
    InitInfo.cAttributesToFetch := 2;
    InitInfo.apwzAttributeNames := @Attributes[0];
    // 5. initialize object picker
    if SUCCEEDED(ObjPicker.Initialize(InitInfo)) then
    begin
      // 6. display dialog
      HR := ObjPicker.InvokeDialog(0, DataObj);
      if HR = S_OK then
      begin
        // 7. user selected one or more objects and clicked OK - process the selection
        Format.cfFormat := RegisterClipboardFormat(CFSTR_DSOP_DS_SELECTION_LIST);
        Format.ptd := nil;
        Format.dwAspect := DVASPECT_CONTENT;
        Format.lindex := -1;
        Format.tymed := TYMED_HGLOBAL;
        FillChar(Medium, SizeOf(Medium), 0);
        Medium.tymed := TYMED_HGLOBAL;
        if SUCCEEDED(DataObj.GetData(Format, Medium)) then
        begin
          SelectionList := GlobalLock(Medium.hGlobal);
          try
            // 8. display generic selection info
            Assert(SelectionList.cItems <> 0);
            Memo1.Lines.Add('Selected objects count: ' + IntToStr(SelectionList.cItems));
            Memo1.Lines.Add('Fetched attributes count: ' + IntToStr(SelectionList.cFetchedAttributes));
            Memo1.Lines.Add('');
            // 9. for each selected object
            for I := 0 to SelectionList.cItems - 1 do
            begin
              {$R-}Selection := SelectionList.aDsSelection[I];{$R+}
              // 10. display the object fields
              Memo1.Lines.Add('Name: ' + string(WideString(Selection.pwzName)));
              Memo1.Lines.Add('ADSPath: ' + string(WideString(Selection.pwzADsPath)));
              Memo1.Lines.Add('Class: ' + string(WideString(Selection.pwzClass)));
              Memo1.Lines.Add('UPN: ' + string(WideString(Selection.pwzUPN)));
              // 11. display the attributes. Note that attribute is an OleVariant that can contain a large number of
              //     different types and formats depending on the requested attributes. we happen to request only string
              //     typed attributes so we can display them without further conversion (OleVariant to string is
              //     automatic). for other types significant programming may be required, but that's outside the scope
              //     of this article..
              Attribute := POleVariantArray(Selection.pvarFetchedAttributes)^[0];
              if VarIsEmpty(Attribute) then
                Memo1.Lines.Add('displayName: <unavailable>')
              else
                Memo1.Lines.Add('displayName: ' + Attribute);
              Attribute := POleVariantArray(Selection.pvarFetchedAttributes)^[1];
              if VarIsEmpty(Attribute) then
                Memo1.Lines.Add('email: <unavailable>')
              else
                Memo1.Lines.Add('email: ' + Attribute);
              Memo1.Lines.Add('');
            end;
          finally
            GlobalUnlock(Medium.hGlobal);
            ReleaseStgMedium(Medium);
          end;
        end;
      end
      else if HR = S_FALSE then
      begin
        // user cancelled the dialog, no selection
        ShowMessage('Dialog cancelled');
      end
      else
      begin
        // an error occurred, the user never saw the dialog, raise an exception
        OleCheck(HR);
      end;
    end;
  end;
  // 12. uninitialize COM
  ObjPicker := nil;
  DataObj := nil;
  CoUninitialize;
end;

end.
