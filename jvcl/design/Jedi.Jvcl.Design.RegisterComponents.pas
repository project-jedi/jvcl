unit Jedi.Jvcl.Design.RegisterComponents;

interface

uses
  SysUtils, Classes;

procedure Register;

implementation

{$R JvCoreReg.dcr}

uses
  // Core
  Jedi.Jvcl.JvPoweredBy,
  Jedi.Jvcl.JvAppStorage,
  Jedi.Jvcl.JvAppIniStorage,
  Jedi.Jvcl.JvAppXMLStorage,
  Jedi.Jvcl.JvAppStorageSelectList,
  Jedi.Jvcl.JvAutoComplete,
  // Components
  Jedi.Jvcl.JvAlarm,
  Jedi.Jvcl.JvConverter,
  Jedi.Jvcl.JvDataEmbedded,
  Jedi.Jvcl.JvEnterTab,
  Jedi.Jvcl.JvMergeManager,          DbClient
  Jedi.Jvcl.JvPageManager;


resourcestring
  RsPaletteNonVisible = 'JVCL.NET Components';
  RsPaletteControls = 'JVCL.NET Controls';

procedure Register;
begin
  // Core
  RegisterComponents(RsPaletteNonVisible, [TJvPoweredByJCL, TJvPoweredByJVCL]);

  RegisterComponents(RsPaletteNonVisible, [TJvAppStorage,
    TJvAppIniFileStorage, TJvAppStorageSelectList, TJvAppXMLFileStorage]);

  RegisterComponents(RsPaletteNonVisible, [TJvLookupAutoComplete]);


  // Components
  RegisterComponents(RsPaletteNonVisual, [TJvAlarms, TJvConverter,
    TJvDataEmbedded,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager{, TJvStrHolder, TJvMultiStringHolder}]);



end;

{$IFDEF RTL170_UP}

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices,AboutBoxServices);
  Assert(Assigned(AboutBoxServices), RsENoAboutServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JVCLSPLASH');
  AboutBoxIndex := AboutBoxServices.AddProductInfo(RsAboutDialogTitle,
    RsAboutCopyright, RsAboutTitle, RsAboutDescription, 0,
    ProductImage, False, RsAboutLicenceStatus);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemoveProductInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  Assert(Assigned(SplashScreenServices), RsENoSplashServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JVCLSPLASH');
  SplashScreenServices.AddProductBitmap(RsAboutDialogTitle,ProductImage,
    False,RsAboutLicenceStatus);
end;

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

{$ENDIF RTL170_UP}

end.
