{$I JVCL.INC}

unit JvAppFrmReg;

interface

procedure Register;

implementation
uses
  Classes, Graphics, {$IFDEF COMPILER6_UP} DesignIntf, {$ELSE} DsgnIntf, {$ENDIF COMPILER6_UP} 
  JvAppAnimatedIcon, JvAppEvent, JvAppHotKey, JvTransparentForm,
  JvFormAnimatedIcon, JvFormAnimation, JvFormWallpaper,
  JvFormMagnet, JvAnimTitle, JvFormAutoSize,
  JvFormWallpaperEditor;

{$R ..\resources\JvAppFrmReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Application and Forms',[
    TJvAppEvents, TJvAppAnimatedIcon, TJvFormAnimatedIcon, TJvApplicationHotKey, TJvTransparentForm,
    TJvFormAnimation, TJvFormWallpaper, TJvFormMagnet, TJvFormAutoSize
    ]);
  RegisterPropertyEditor(TypeInfo(TPicture), TJvFormWallpaper, 'Image', TJvFormWallpaperEditor);
end;

end.
