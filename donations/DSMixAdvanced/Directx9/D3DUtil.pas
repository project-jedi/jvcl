(*----------------------------------------------------------------------------*
 *  DirectX 9 C++ common framework adaptation for Delphi by Alexey Barkovoy   *
 *  E-Mail: clootie@reactor.ru                                                *
 *                                                                            *
 *  Desc: Direct3D part of framework.                                         *
 *  Delphi versions 5-7 are supported                                         *
 *                                                                            *
 *  Modified: 25-Dec-2002                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://clootie.narod.ru/delphi                                         *
 *----------------------------------------------------------------------------*)
//-----------------------------------------------------------------------------
// File: D3DUtil.h
//
// Desc: Helper functions and typing shortcuts for Direct3D programming.
//
// Copyright (c) Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------
unit D3DUtil;

{$I DirectX.inc}

interface

uses
  Windows, Messages, Direct3D9, D3DX9;

//-----------------------------------------------------------------------------
// Name: D3DUtil_InitMaterial()
// Desc: Initializes a D3DMATERIAL9 structure, setting the diffuse and ambient
//       colors. It does not set emissive or specular colors.
//-----------------------------------------------------------------------------
procedure D3DUtil_InitMaterial(var mtrl: TD3DMaterial9;
  r: Single = 0.0;
  g: Single = 0.0;
  b: Single = 0.0;
  a: Single = 1.0);


//-----------------------------------------------------------------------------
// Name: D3DUtil_InitLight()
// Desc: Initializes a D3DLIGHT structure, setting the light position. The
//       diffuse color is set to white, specular and ambient left as black.
//-----------------------------------------------------------------------------
procedure D3DUtil_InitLight(var light: TD3DLight9; ltType: TD3DLightType;
  x: Single = 0.0;
  y: Single = 0.0;
  z: Single = 0.0);


//-----------------------------------------------------------------------------
// Name: D3DUtil_CreateTexture()
// Desc: Helper function to create a texture. It checks the root path first,
//       then tries the DXSDK media path (as specified in the system registry).
//-----------------------------------------------------------------------------
function D3DUtil_CreateTexture(pd3dDevice: IDirect3DDevice9; strTexture: PChar;
  var ppTexture: IDirect3DTexture9;
  d3dFormat: TD3DFormat = D3DFMT_UNKNOWN): HRESULT;




//-----------------------------------------------------------------------------
// Name: D3DUtil_GetCubeMapViewMatrix()
// Desc: Returns a view matrix for rendering to a face of a cubemap.
//-----------------------------------------------------------------------------
function D3DUtil_GetCubeMapViewMatrix(dwFace: TD3DCubemapFaces): TD3DXMatrix;


//-----------------------------------------------------------------------------
// Name: D3DUtil_GetRotationFromCursor()
// Desc: Returns a quaternion for the rotation implied by the window's cursor
//       position.
//-----------------------------------------------------------------------------
function D3DUtil_GetRotationFromCursor(hWnd_: HWND;
  fTrackBallRadius: Single = 1.0): TD3DXQuaternion;


//-----------------------------------------------------------------------------
// Name: D3DUtil_SetDeviceCursor
// Desc: Builds and sets a cursor for the D3D device based on hCursor.
//-----------------------------------------------------------------------------
function D3DUtil_SetDeviceCursor(pd3dDevice: IDirect3DDevice9; hCursor: HCURSOR;
  bAddWatermark: BOOL): HRESULT;


//-----------------------------------------------------------------------------
// Name: D3DUtil_D3DFormatToString
// Desc: Returns the string for the given D3DFORMAT.
//       bWithPrefix determines whether the string should include the "D3DFMT_"
//-----------------------------------------------------------------------------
function D3DUtil_D3DFormatToString(format: TD3DFormat; bWithPrefix: Boolean = True): PChar; // overload;
//todo: Check this too
//function D3DUtil_D3DFormatToString(format: TD3DFormat; bWithPrefix2: Boolean = True): Byte; overload;


type
  //-----------------------------------------------------------------------------
  // Name: class CD3DArcBall
  // Desc:
  //-----------------------------------------------------------------------------
  CD3DArcBall = class

    m_iWidth: Integer;                     // ArcBall's window width
    m_iHeight: Integer;                    // ArcBall's window height
    m_fRadius: Single;                     // ArcBall's radius in screen coords
    m_fRadiusTranslation: Single;          // ArcBall's radius for translating the target

    m_qDown: TD3DXQuaternion;              // Quaternion before button down
    m_qNow: TD3DXQuaternion;               // Composite quaternion for current drag
    m_matRotation: TD3DXMatrixA16;         // Matrix for arcball's orientation
    m_matRotationDelta: TD3DXMatrixA16;    // Matrix for arcball's orientation
    m_matTranslation: TD3DXMatrixA16;      // Matrix for arcball's position
    m_matTranslationDelta: TD3DXMatrixA16; // Matrix for arcball's position
    m_bDrag: Boolean;                      // Whether user is dragging arcball
    m_bRightHanded: Boolean;               // Whether to use RH coordinate system

    function ScreenToVector(sx, sy: Integer): TD3DXVector3;

  public
    function HandleMouseMessages(hWnd: HWND; uMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): LRESULT; overload;
    procedure HandleMouseMessages(var Msg: TMsg; var Handled: Boolean); overload;

    function GetRotationMatrix: PD3DXMatrix;         { return &m_matRotation; }
    function GetRotationDeltaMatrix: PD3DXMatrix;    { return &m_matRotationDelta; }
    function GetTranslationMatrix: PD3DXMatrix;      { return &m_matTranslation; }
    function GetTranslationDeltaMatrix: PD3DXMatrix; { return &m_matTranslationDelta; }
    function IsBeingDragged: Boolean;                { return m_bDrag; }

    procedure SetRadius(fRadius: Single);
    procedure SetWindow(w, h: Integer; r: Single = 0.9);
    procedure SetRightHanded(bRightHanded: BOOL); // { m_bRightHanded = bRightHanded; }

    constructor Create;
    procedure Init;
  end;




  //-----------------------------------------------------------------------------
  // Name: class CD3DCamera
  // Desc:
  //-----------------------------------------------------------------------------
  CD3DCamera = class

    m_vEyePt: TD3DXVector3;       // Attributes for view matrix
    m_vLookatPt: TD3DXVector3;
    m_vUpVec: TD3DXVector3;

    m_vView: TD3DXVector3;
    m_vCross: TD3DXVector3;

    m_matView: TD3DXMatrixA16;
    m_matBillboard: TD3DXMatrixA16;  // Special matrix for billboarding effects

    m_fFOV: Single;               // Attributes for projection matrix
    m_fAspect: Single;
    m_fNearPlane: Single;
    m_fFarPlane: Single;
    m_matProj: TD3DXMatrixA16;

  public
    // Access functions
    property EyePt: TD3DXVector3              read m_vEyePt;
    property LookatPt: TD3DXVector3           read m_vLookatPt;
    property UpVec: TD3DXVector3              read m_vUpVec;
    property ViewDir: TD3DXVector3            read m_vView;
    property Cross: TD3DXVector3              read m_vCross;

    property FOV: Single                      read m_fFOV;
    property Aspect: Single                   read m_fAspect;
    property NearPlane: Single                read m_fNearPlane;
    property FarPlane: Single                 read m_fFarPlane;

    property ViewMatrix: TD3DXMatrix          read m_matView;
    property BillboardMatrix: TD3DXMatrix     read m_matBillboard;
    property ProjMatrix: TD3DXMatrix          read m_matProj;
  public

    procedure SetViewParams(const vEyePt, vLookatPt, vUpVec: TD3DXVector3);
    procedure SetProjParams(fFOV, fAspect, fNearPlane, fFarPlane: Single);

    constructor Create;
  end;

  
implementation

uses Math, DXUtil;


//-----------------------------------------------------------------------------
// Name: D3DUtil_InitMaterial()
// Desc: Initializes a D3DMATERIAL9 structure, setting the diffuse and ambient
//       colors. It does not set emissive or specular colors.
//-----------------------------------------------------------------------------
procedure D3DUtil_InitMaterial(var mtrl: TD3DMaterial9; r: Single; g: Single;
                                                        b: Single; a: Single);
begin
    ZeroMemory(@mtrl, SizeOf(TD3DMaterial9));
    mtrl.Diffuse.r := r;
    mtrl.Ambient.r := r;
    mtrl.Diffuse.g := g;
    mtrl.Ambient.g := g;
    mtrl.Diffuse.b := b;
    mtrl.Ambient.b := b;
    mtrl.Diffuse.a := a;
    mtrl.Ambient.a := a;
end;



//-----------------------------------------------------------------------------
// Name: D3DUtil_InitLight()
// Desc: Initializes a D3DLIGHT structure, setting the light position. The
//       diffuse color is set to white; specular and ambient are left as black.
//-----------------------------------------------------------------------------
procedure D3DUtil_InitLight(var light: TD3DLight9; ltType: TD3DLightType;
  x: Single; y: Single; z: Single);
var
  vecLightDirUnnormalized: TD3DXVector3;
begin
  vecLightDirUnnormalized:= D3DXVector3(x, y, z);
  ZeroMemory(@light, SizeOf(TD3DLight9));
  light._Type       := ltType;
  light.Diffuse.r   := 1.0;
  light.Diffuse.g   := 1.0;
  light.Diffuse.b   := 1.0;
  D3DXVec3Normalize(light.Direction, vecLightDirUnnormalized);
  light.Position.x  := x;
  light.Position.y  := y;
  light.Position.z  := z;
  light.Range       := 1000.0;
end;



//-----------------------------------------------------------------------------
// Name: D3DUtil_CreateTexture()
// Desc: Helper function to create a texture. It checks the root path first,
//       then tries the DXSDK media path (as specified in the system registry).
//-----------------------------------------------------------------------------
function D3DUtil_CreateTexture(pd3dDevice: IDirect3DDevice9; strTexture: PChar;
  var ppTexture: IDirect3DTexture9; d3dFormat: TD3DFormat): HRESULT;
var
  strPath: array[0..MAX_PATH-1] of Char;
begin
  // Get the path to the texture
  Result:= DXUtil_FindMediaFileCb(strPath, SizeOf(strPath), strTexture);
  if FAILED(Result) then Exit;

  // Create the texture using D3DX
  Result:= D3DXCreateTextureFromFileEx(pd3dDevice, strPath,
              D3DX_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, d3dFormat,
              D3DPOOL_MANAGED, D3DX_FILTER_TRIANGLE or D3DX_FILTER_MIRROR,
              D3DX_FILTER_TRIANGLE or D3DX_FILTER_MIRROR, 0, nil, nil, ppTexture);
end;



//-----------------------------------------------------------------------------
// Name: D3DUtil_GetCubeMapViewMatrix()
// Desc: Returns a view matrix for rendering to a face of a cubemap.
//-----------------------------------------------------------------------------
function D3DUtil_GetCubeMapViewMatrix(dwFace: TD3DCubemapFaces): TD3DXMatrix;
var
  vEyePt: TD3DXVector3;
  vLookDir: TD3DXVector3;
  vUpDir: TD3DXVector3;
  matView: TD3DXMatrixA16;
begin
  vEyePt:= D3DXVector3(0.0, 0.0, 0.0);

  case dwFace of
    D3DCUBEMAP_FACE_POSITIVE_X:
    begin
      vLookDir := D3DXVector3( 1.0, 0.0, 0.0 );
      vUpDir   := D3DXVector3( 0.0, 1.0, 0.0 );
    end;
    D3DCUBEMAP_FACE_NEGATIVE_X:
    begin
      vLookDir := D3DXVector3(-1.0, 0.0, 0.0 );
      vUpDir   := D3DXVector3( 0.0, 1.0, 0.0 );
    end;
    D3DCUBEMAP_FACE_POSITIVE_Y:
    begin
      vLookDir := D3DXVector3( 0.0, 1.0, 0.0 );
      vUpDir   := D3DXVector3( 0.0, 0.0,-1.0 );
    end;
    D3DCUBEMAP_FACE_NEGATIVE_Y:
    begin
      vLookDir := D3DXVector3( 0.0,-1.0, 0.0 );
      vUpDir   := D3DXVector3( 0.0, 0.0, 1.0 );
    end;
    D3DCUBEMAP_FACE_POSITIVE_Z:
    begin
      vLookDir := D3DXVector3( 0.0, 0.0, 1.0 );
      vUpDir   := D3DXVector3( 0.0, 1.0, 0.0 );
    end;
    D3DCUBEMAP_FACE_NEGATIVE_Z:
    begin
      vLookDir := D3DXVector3( 0.0, 0.0,-1.0 );
      vUpDir   := D3DXVector3( 0.0, 1.0, 0.0 );
    end;
  end;

  // Set the view transform for this cubemap surface
  D3DXMatrixLookAtLH(matView, vEyePt, vLookDir, vUpDir);
  Result:= matView;
end;



//-----------------------------------------------------------------------------
// Name: D3DUtil_GetRotationFromCursor()
// Desc: Returns a quaternion for the rotation implied by the window's cursor
//       position.
//-----------------------------------------------------------------------------
function D3DUtil_GetRotationFromCursor(hWnd_: HWND; fTrackBallRadius: Single): TD3DXQuaternion;
var
  pt: TPoint;
  rc: TRect;
  sx, sy, sz, d2, t, fAngle: Single;
  p1: TD3DXVector3;
  p2: TD3DXVector3;
  vAxis: TD3DXVector3;
  quat: TD3DXQuaternion;
  v: TD3DXVector3; // temporary vector
  vecDiff: TD3DXVector3;
begin
  GetCursorPos(pt);
  GetClientRect(hWnd_, rc);
  ScreenToClient(hWnd_, pt);
  sx := ((2.0 * pt.x) / (rc.right-rc.left)) - 1;
  sy := ((2.0 * pt.y) / (rc.bottom-rc.top)) - 1;

  if (sx = 0.0) and (sy = 0.0) then
  begin
    Result:= D3DXQuaternion(0.0, 0.0, 0.0, 1.0);
    Exit;
  end;

  d2:= Sqrt(sx*sx + sy*sy);

  if (d2 < fTrackBallRadius * 0.70710678118654752440) then // Inside sphere
    sz := Sqrt(fTrackBallRadius*fTrackBallRadius - d2*d2)
  else                                                     // On hyperbola
    sz := (fTrackBallRadius*fTrackBallRadius) / (2.0*d2);

  // Get two points on trackball's sphere
  p1:= D3DXVector3(sx, sy, sz);
  p2:= D3DXVector3(0.0, 0.0, fTrackBallRadius);

  // Get axis of rotation, which is cross product of p1 and p2
  D3DXVec3Cross(vAxis, p1, p2);

  // Calculate angle for the rotation about that axis
  D3DXVec3Subtract(vecDiff, p2, p1);
  // FLOAT t = D3DXVec3Length( &vecDiff ) / ( 2.0f*fTrackBallRadius );
  t:= D3DXVec3Length(D3DXVec3Scale(v, vecDiff, 1 / (2.0*fTrackBallRadius))^);
  if (t > +1.0) then t := +1.0;
  if (t < -1.0) then t := -1.0;
  fAngle := 2.0 * ArcSin(t);

  // Convert axis to quaternion
  D3DXQuaternionRotationAxis(quat, vAxis, fAngle);
  Result:= quat;
end;



//-----------------------------------------------------------------------------
// Name: D3DUtil_SetDeviceCursor
// Desc: Gives the D3D device a cursor with image and hotspot from hCursor.
//-----------------------------------------------------------------------------
function D3DUtil_SetDeviceCursor(pd3dDevice: IDirect3DDevice9; hCursor: HCURSOR;
  bAddWatermark: BOOL): HRESULT;
const
  wMask: array [0..4] of Word = ($ccc0, $a2a0, $a4a0, $a2a0, $ccc0);
label
  End_;
type
  PACOLORREF = ^ACOLORREF;
  ACOLORREF = array[0..0] of COLORREF;
type
  pImg = ^img;
  img = array[0..16000] of DWORD;
var
  hr: HRESULT;
  iconinfo_: TIconInfo;
  bBWCursor: BOOL;
  pCursorSurface: IDirect3DSurface9;
  hdcColor: HDC;
  hdcMask: HDC;
  hdcScreen: HDC;
  bm: TBitmap;
  dwWidth: DWORD;
  dwHeightSrc: DWORD;
  dwHeightDest: DWORD;
  crColor: COLORREF;
  crMask: COLORREF;
  x,y: Cardinal;
  bmi: TBitmapInfo;
  pcrArrayColor: PACOLORREF;
  pcrArrayMask: PACOLORREF;
  pBitmap: pImg;
  hgdiobjOld: HGDIOBJ;
  lr: TD3DLockedRect;
begin
  hr := E_FAIL;
  pCursorSurface := nil;
  hdcColor := 0;
  hdcMask := 0;
  hdcScreen := 0;
  pcrArrayColor := nil;
  pcrArrayMask := nil;

  ZeroMemory(@iconinfo_, SizeOf(TIconInfo));
  if not GetIconInfo(hCursor, iconinfo_) then
    goto End_;

  if (0 = GetObject(iconinfo_.hbmMask, SizeOf(TBitmap), @bm)) then
    goto End_;
  dwWidth := bm.bmWidth;
  dwHeightSrc := bm.bmHeight;

  if (iconinfo_.hbmColor = 0) then
  begin
    bBWCursor := TRUE;
    dwHeightDest := dwHeightSrc div 2;
  end else
  begin
    bBWCursor := FALSE;
    dwHeightDest := dwHeightSrc;
  end;

  // Create a surface for the fullscreen cursor
  hr:= pd3dDevice.CreateOffscreenPlainSurface(dwWidth, dwHeightDest,
          D3DFMT_A8R8G8B8, D3DPOOL_SCRATCH, pCursorSurface, nil);
  if FAILED(hr) then
    goto End_;

  // pcrArrayMask = new DWORD[dwWidth * dwHeightSrc];
  GetMem(pcrArrayMask, SizeOf(DWORD)*(dwWidth * dwHeightSrc));

  ZeroMemory(@bmi, sizeof(bmi));
  bmi.bmiHeader.biSize := sizeof(bmi.bmiHeader);
  bmi.bmiHeader.biWidth := dwWidth;
  bmi.bmiHeader.biHeight := dwHeightSrc;
  bmi.bmiHeader.biPlanes := 1;
  bmi.bmiHeader.biBitCount := 32;
  bmi.bmiHeader.biCompression := BI_RGB;

  hdcScreen := GetDC(0);
  hdcMask := CreateCompatibleDC(hdcScreen);
  if (hdcMask = 0) then
  begin
    hr := E_FAIL;
    goto End_;
  end;
  hgdiobjOld := SelectObject(hdcMask, iconinfo_.hbmMask);
  GetDIBits(hdcMask, iconinfo_.hbmMask, 0, dwHeightSrc, pcrArrayMask, bmi,
    DIB_RGB_COLORS);
  SelectObject(hdcMask, hgdiobjOld);

  if (not bBWCursor) then
  begin
    // pcrArrayColor = new DWORD[dwWidth * dwHeightDest];
    GetMem(pcrArrayColor, SizeOf(DWORD)*(dwWidth * dwHeightDest));
    hdcColor := CreateCompatibleDC(hdcScreen);
    if (hdcColor = 0) then
    begin
      hr := E_FAIL;
      goto End_;
    end;
    SelectObject(hdcColor, iconinfo_.hbmColor);
    GetDIBits(hdcColor, iconinfo_.hbmColor, 0, dwHeightDest, pcrArrayColor, bmi,
      DIB_RGB_COLORS);
  end;

  // Transfer cursor image into the surface
  pCursorSurface.LockRect(lr, nil, 0);
  pBitmap:= lr.pBits;
  for y:= 0 to dwHeightDest - 1 do
  begin
    for x:= 0 to dwWidth - 1 do
    begin
      if bBWCursor then
      begin
        crColor:= pcrArrayMask^[dwWidth*(dwHeightDest-1-y) + x];
        crMask:= pcrArrayMask^[dwWidth*(dwHeightSrc-1-y) + x];
      end else
      begin
        crColor:= pcrArrayColor^[dwWidth*(dwHeightDest-1-y) + x];
        crMask:= pcrArrayMask^[dwWidth*(dwHeightDest-1-y) + x];
      end;
      if (crMask = 0) then
        pBitmap^[dwWidth*y + x]:= $ff000000 or crColor
      else
        pBitmap^[dwWidth*y + x]:= $00000000;

      // It may be helpful to make the D3D cursor look slightly
      // different from the Windows cursor so you can distinguish
      // between the two when developing/testing code.  When
      // bAddWatermark is TRUE, the following code adds some
      // small grey "D3D" characters to the upper-left corner of
      // the D3D cursor image.

      //if( bAddWatermark && x < 12 && y < 5 )
      if bAddWatermark and (x < 12) and (y < 5) then
      begin
          // 11.. 11.. 11.. .... CCC0
          // 1.1. ..1. 1.1. .... A2A0
          // 1.1. .1.. 1.1. .... A4A0
          // 1.1. ..1. 1.1. .... A2A0
          // 11.. 11.. 11.. .... CCC0

          // if( wMask[y] & (1 << (15 - x)) )
          if (wMask[y] and (1 shl (15 - x)) <> 0) then
          begin
            pBitmap[dwWidth*y + x]:= pBitmap[dwWidth*y + x] or $ff808080;
          end;
      end;
    end;
  end;
  pCursorSurface.UnlockRect;

  // Set the device cursor
  hr := pd3dDevice.SetCursorProperties(iconinfo_.xHotspot,
      iconinfo_.yHotspot, pCursorSurface);
  if FAILED(hr) then
    goto End_;

  hr := S_OK;

End_:
  if (iconinfo_.hbmMask <> 0)  then DeleteObject(iconinfo_.hbmMask);
  if (iconinfo_.hbmColor <> 0) then DeleteObject(iconinfo_.hbmColor);
  if (hdcScreen <> 0)          then ReleaseDC(0, hdcScreen);
  if (hdcColor <> 0)           then DeleteDC(hdcColor);
  if (hdcMask <> 0)            then DeleteDC(hdcMask);
  // SAFE_DELETE_ARRAY(pcrArrayColor);
  FreeMem(pcrArrayColor);
  // SAFE_DELETE_ARRAY(pcrArrayMask);
  FreeMem(pcrArrayMask);
  SAFE_RELEASE(pCursorSurface);
  Result:= hr;
end;



//-----------------------------------------------------------------------------
// Name: D3DFormatToString
// Desc: Returns the string for the given D3DFORMAT.
//-----------------------------------------------------------------------------
function D3DUtil_D3DFormatToString(format: TD3DFormat; bWithPrefix: Boolean = True): PChar; overload;
begin
  case format of
    D3DFMT_UNKNOWN:         Result:= 'D3DFMT_UNKNOWN';
    D3DFMT_R8G8B8:          Result:= 'D3DFMT_R8G8B8';
    D3DFMT_A8R8G8B8:        Result:= 'D3DFMT_A8R8G8B8';
    D3DFMT_X8R8G8B8:        Result:= 'D3DFMT_X8R8G8B8';
    D3DFMT_R5G6B5:          Result:= 'D3DFMT_R5G6B5';
    D3DFMT_X1R5G5B5:        Result:= 'D3DFMT_X1R5G5B5';
    D3DFMT_A1R5G5B5:        Result:= 'D3DFMT_A1R5G5B5';
    D3DFMT_A4R4G4B4:        Result:= 'D3DFMT_A4R4G4B4';
    D3DFMT_R3G3B2:          Result:= 'D3DFMT_R3G3B2';
    D3DFMT_A8:              Result:= 'D3DFMT_A8';
    D3DFMT_A8R3G3B2:        Result:= 'D3DFMT_A8R3G3B2';
    D3DFMT_X4R4G4B4:        Result:= 'D3DFMT_X4R4G4B4';
    D3DFMT_A2B10G10R10:     Result:= 'D3DFMT_A2B10G10R10';
    D3DFMT_A8B8G8R8:        Result:= 'D3DFMT_A8B8G8R8';
    D3DFMT_X8B8G8R8:        Result:= 'D3DFMT_X8B8G8R8';
    D3DFMT_G16R16:          Result:= 'D3DFMT_G16R16';
    D3DFMT_A2R10G10B10:     Result:= 'D3DFMT_A2R10G10B10';
    D3DFMT_A16B16G16R16:    Result:= 'D3DFMT_A16B16G16R16';
    D3DFMT_A8P8:            Result:= 'D3DFMT_A8P8';
    D3DFMT_P8:              Result:= 'D3DFMT_P8';
    D3DFMT_L8:              Result:= 'D3DFMT_L8';
    D3DFMT_A8L8:            Result:= 'D3DFMT_A8L8';
    D3DFMT_A4L4:            Result:= 'D3DFMT_A4L4';
    D3DFMT_V8U8:            Result:= 'D3DFMT_V8U8';
    D3DFMT_L6V5U5:          Result:= 'D3DFMT_L6V5U5';
    D3DFMT_X8L8V8U8:        Result:= 'D3DFMT_X8L8V8U8';
    D3DFMT_Q8W8V8U8:        Result:= 'D3DFMT_Q8W8V8U8';
    D3DFMT_V16U16:          Result:= 'D3DFMT_V16U16';
    D3DFMT_A2W10V10U10:     Result:= 'D3DFMT_A2W10V10U10';
    D3DFMT_UYVY:            Result:= 'D3DFMT_UYVY';
    D3DFMT_YUY2:            Result:= 'D3DFMT_YUY2';
    D3DFMT_DXT1:            Result:= 'D3DFMT_DXT1';
    D3DFMT_DXT2:            Result:= 'D3DFMT_DXT2';
    D3DFMT_DXT3:            Result:= 'D3DFMT_DXT3';
    D3DFMT_DXT4:            Result:= 'D3DFMT_DXT4';
    D3DFMT_DXT5:            Result:= 'D3DFMT_DXT5';
    D3DFMT_D16_LOCKABLE:    Result:= 'D3DFMT_D16_LOCKABLE';
    D3DFMT_D32:             Result:= 'D3DFMT_D32';
    D3DFMT_D15S1:           Result:= 'D3DFMT_D15S1';
    D3DFMT_D24S8:           Result:= 'D3DFMT_D24S8';
    D3DFMT_D24X8:           Result:= 'D3DFMT_D24X8';
    D3DFMT_D24X4S4:         Result:= 'D3DFMT_D24X4S4';
    D3DFMT_D16:             Result:= 'D3DFMT_D16';
    D3DFMT_L16:             Result:= 'D3DFMT_L16';
    D3DFMT_VERTEXDATA:      Result:= 'D3DFMT_VERTEXDATA';
    D3DFMT_INDEX16:         Result:= 'D3DFMT_INDEX16';
    D3DFMT_INDEX32:         Result:= 'D3DFMT_INDEX32';
    D3DFMT_Q16W16V16U16:    Result:= 'D3DFMT_Q16W16V16U16';
    D3DFMT_MULTI2_ARGB8:    Result:= 'D3DFMT_MULTI2_ARGB8';
    D3DFMT_R16F:            Result:= 'D3DFMT_R16F';
    D3DFMT_G16R16F:         Result:= 'D3DFMT_G16R16F';
    D3DFMT_A16B16G16R16F:   Result:= 'D3DFMT_A16B16G16R16F';
    D3DFMT_R32F:            Result:= 'D3DFMT_R32F';
    D3DFMT_G32R32F:         Result:= 'D3DFMT_G32R32F';
    D3DFMT_A32B32G32R32F:   Result:= 'D3DFMT_A32B32G32R32F';
    D3DFMT_CxV8U8:          Result:= 'D3DFMT_CxV8U8';
  else
    Result:= 'Unknown format';
  end;
  if (bWithPrefix or (Pos('D3DFMT_', Result) = 0)) then {nothing}
  else Result:= Result + Length('D3DFMT_');
end;



//-----------------------------------------------------------------------------
// Name: D3DXQuaternionUnitAxisToUnitAxis2
// Desc: Axis to axis quaternion double angle (no normalization)
//       Takes two points on unit sphere an angle THETA apart, returns
//       quaternion that represents a rotation around cross product by 2*THETA.
//-----------------------------------------------------------------------------
{inline} function D3DXQuaternionUnitAxisToUnitAxis2(out pOut: TD3DXQuaternion;
           const pvFrom, pvTo: TD3DXVector3): PD3DXQuaternion; // WINAPI;
var
  vAxis: TD3DXVector3;
begin
  D3DXVec3Cross(vAxis, pvFrom, pvTo);    // proportional to sin(theta)
  pOut.x := vAxis.x;
  pOut.y := vAxis.y;
  pOut.z := vAxis.z;
  pOut.w := D3DXVec3Dot(pvFrom, pvTo);
  Result:= @pOut;
end;



//-----------------------------------------------------------------------------
// Name: D3DXQuaternionAxisToAxis
// Desc: Axis to axis quaternion
//       Takes two points on unit sphere an angle THETA apart, returns
//       quaternion that represents a rotation around cross product by theta.
//-----------------------------------------------------------------------------
{inline} function D3DXQuaternionAxisToAxis(out pOut: TD3DXQuaternion;
           const pvFrom, pvTo: TD3DXVector3): PD3DXQuaternion; // WINAPI;
var
  vA, vB: TD3DXVector3;
  vHalf: TD3DXVector3;
begin
  D3DXVec3Normalize(vA, pvFrom);
  D3DXVec3Normalize(vB, pvTo);
  D3DXVec3Add(vHalf, vA, vB);
  D3DXVec3Normalize(vHalf, vHalf);
  Result:= D3DXQuaternionUnitAxisToUnitAxis2(pOut, vA, vHalf);
end;




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
constructor CD3DArcBall.Create;
begin
  Init;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DArcBall.Init;
begin
  D3DXQuaternionIdentity(m_qDown);
  D3DXQuaternionIdentity(m_qNow);
  D3DXMatrixIdentity(m_matRotation);
  D3DXMatrixIdentity(m_matRotationDelta);
  D3DXMatrixIdentity(m_matTranslation);
  D3DXMatrixIdentity(m_matTranslationDelta);
  m_bDrag := False;
  m_fRadiusTranslation := 1.0;
  m_bRightHanded := False;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DArcBall.SetWindow(w, h: Integer; r: Single);
begin
  // Set ArcBall info
  m_iWidth  := w;
  m_iHeight := h;
  m_fRadius := r;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DArcBall.SetRightHanded(bRightHanded: BOOL);
begin
  m_bRightHanded:= bRightHanded;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DArcBall.ScreenToVector(sx, sy: Integer): TD3DXVector3;
var
  x, y, z, mag, scale: Single;
begin
  // Scale to screen
  x   := -(sx - m_iWidth/2)  / (m_fRadius*m_iWidth/2);
  y   :=  (sy - m_iHeight/2) / (m_fRadius*m_iHeight/2);

  if m_bRightHanded then
  begin
    x := -x;
    y := -y;
  end;

  z   := 0.0;
  mag := x*x + y*y;

  if (mag > 1.0) then
  begin
    scale := 1.0 / Sqrt(mag);
    x := x*scale;
    y := y*scale;
  end
  else
    z := Sqrt(1.0 - mag);

  // Return vector
  Result:= D3DXVector3(x, y, z);
end;


function CD3DArcBall.GetRotationMatrix: PD3DXMatrix;         { return &m_matRotation; }
begin
  Result:= @m_matRotation;
end;

function CD3DArcBall.GetRotationDeltaMatrix: PD3DXMatrix;    { return &m_matRotationDelta; }
begin
  Result:= @m_matRotationDelta;
end;

function CD3DArcBall.GetTranslationMatrix: PD3DXMatrix;      { return &m_matTranslation; }
begin
  Result:= @m_matTranslation;
end;

function CD3DArcBall.GetTranslationDeltaMatrix: PD3DXMatrix; { return &m_matTranslationDelta; }
begin
  Result:= @m_matTranslationDelta;
end;

function CD3DArcBall.IsBeingDragged: Boolean;                { return m_bDrag; }
begin
  Result:= m_bDrag;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DArcBall.SetRadius(fRadius: Single);
begin
  m_fRadiusTranslation:= fRadius;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DArcBall.HandleMouseMessages(
  hWnd: HWND; uMsg: Cardinal; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  Msg: TMsg;
  Handled: Boolean;
begin
  Msg.hwnd:= hWnd;
  Msg.message:= uMsg;
  Msg.wParam:= wParam;
  Msg.lParam:= lParam;

  HandleMouseMessages(Msg, Handled);
  Result:= Integer(Handled);
end;

procedure CD3DArcBall.HandleMouseMessages(
  var Msg: TMsg; var Handled: Boolean);
{$WRITEABLECONST ON}
const
  iCurMouseX: Integer = 0;                         // Saved mouse position
  iCurMouseY: Integer = 0;
  s_vDown: TD3DXVector3 = (x:0; y:0; z:0);         // Button down vector
{$WRITEABLECONST OFF}
var
  // Current mouse position
  iMouseX: Integer;
  iMouseY: Integer;
  vCur: TD3DXVector3;
  fDeltaX, fDeltaY: Single;
  qAxisToAxis: TD3DXQuaternion;
// #define GET_X_LPARAM(lp)                        ((int)(short)LOWORD(lp))
// #define GET_Y_LPARAM(lp)                        ((int)(short)HIWORD(lp))
begin
  // Current mouse position
  iMouseX := Integer(SmallInt(LOWORD(Msg.lParam))); // GET_X_LPARAM(Msg.lParam);
  iMouseY := Integer(SmallInt(HIWORD(Msg.lParam))); // GET_Y_LPARAM(Msg.lParam);

  case (Msg.message) of
    WM_RBUTTONDOWN,
    WM_MBUTTONDOWN:
    begin
      // Store off the position of the cursor when the button is pressed
      iCurMouseX:= iMouseX;
      iCurMouseY:= iMouseY;
      Handled:= TRUE;
      Exit;
    end;

    WM_LBUTTONDOWN:
    begin
      // Start drag mode
      m_bDrag:= True;
      s_vDown:= ScreenToVector(iMouseX, iMouseY);
      m_qDown:= m_qNow;
      Handled:= TRUE;
      Exit;
    end;

    WM_LBUTTONUP:
    begin
      // End drag mode
      m_bDrag:= False;
      Handled:= True;
      Exit;
    end;

    WM_MOUSEMOVE:
    begin
      // Drag object
      if ((MK_LBUTTON and Msg.wParam) = MK_LBUTTON) then
      begin
        if m_bDrag then
        begin
          // recompute m_qNow
          vCur:= ScreenToVector(iMouseX, iMouseY);
          D3DXQuaternionAxisToAxis(qAxisToAxis, s_vDown, vCur);
          m_qNow := m_qDown;
          // m_qNow *= qAxisToAxis;
          D3DXQuaternionMultiply(m_qNow, m_qNow, qAxisToAxis);
          D3DXMatrixRotationQuaternion(m_matRotationDelta, qAxisToAxis);
        end else
          D3DXMatrixIdentity(m_matRotationDelta);

        D3DXMatrixRotationQuaternion(m_matRotation, m_qNow);
        m_bDrag:= True;
      end
      else if ((MK_RBUTTON and Msg.wParam) = MK_RBUTTON) or
              ((MK_MBUTTON and Msg.wParam) = MK_MBUTTON) then
      begin
        // Normalize based on size of window and bounding sphere radius
        fDeltaX:= (iCurMouseX - iMouseX) * m_fRadiusTranslation / m_iWidth;
        fDeltaY:= (iCurMouseY - iMouseY) * m_fRadiusTranslation / m_iHeight;

        if (Msg.wParam and MK_RBUTTON) = MK_RBUTTON then
        begin
          D3DXMatrixTranslation(m_matTranslationDelta, -2*fDeltaX, 2*fDeltaY, 0.0);
          D3DXMatrixMultiply(m_matTranslation, m_matTranslation, m_matTranslationDelta);
        end
        else  // wParam & MK_MBUTTON
        begin
          D3DXMatrixTranslation(m_matTranslationDelta, 0.0, 0.0, 5*fDeltaY);
          D3DXMatrixMultiply(m_matTranslation, m_matTranslation, m_matTranslationDelta);
        end;

        // Store mouse coordinate
        iCurMouseX:= iMouseX;
        iCurMouseY:= iMouseY;
      end;
      Handled:= True;
      Exit;
    end;
  end;

  Handled:= False;
end;




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
constructor CD3DCamera.Create;
var
  v1,v2,v3:TD3DXVector3;
begin
  // Set attributes for the view matrix
  v1:= D3DXVector3(0.0,0.0,0.0);
  v2:= D3DXVector3(0.0,0.0,1.0);
  v3:= D3DXVector3(0.0,1.0,0.0);
  SetViewParams(v1, v2, v3);

  // Set attributes for the projection matrix
  SetProjParams(D3DX_PI/4, 1.0, 1.0, 1000.0);
end;




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DCamera.SetViewParams(const vEyePt, vLookatPt, vUpVec: TD3DXVector3);
var
  vDir: TD3DXVector3;
begin
  // Set attributes for the view matrix
  m_vEyePt    := vEyePt;
  m_vLookatPt := vLookatPt;
  m_vUpVec    := vUpVec;
  D3DXVec3Subtract(vDir, m_vLookatPt , m_vEyePt);
  D3DXVec3Normalize(m_vView, vDir);
  D3DXVec3Cross(m_vCross, m_vView, m_vUpVec);

  D3DXMatrixLookAtLH(m_matView, m_vEyePt, m_vLookatPt, m_vUpVec);
  D3DXMatrixInverse(m_matBillboard, nil, m_matView);
  m_matBillboard._41 := 0.0;
  m_matBillboard._42 := 0.0;
  m_matBillboard._43 := 0.0;
end;




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DCamera.SetProjParams(fFOV, fAspect, fNearPlane, fFarPlane: Single);
begin
  // Set attributes for the projection matrix
  m_fFOV        := fFOV;
  m_fAspect     := fAspect;
  m_fNearPlane  := fNearPlane;
  m_fFarPlane   := fFarPlane;

  D3DXMatrixPerspectiveFovLH(m_matProj, fFOV, fAspect, fNearPlane, fFarPlane);
end;

end.

