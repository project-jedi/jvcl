{$I JVCL.INC}

unit JvAppFrmReg;

interface

procedure Register;

implementation
uses
  Classes, Graphics, DesignIntf, 
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
