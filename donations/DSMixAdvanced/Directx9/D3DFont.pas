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
// File: D3DFont.h D3DFont.cpp
//
// Desc: Texture-based font class
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
unit D3DFont;

{$I DirectX.inc}

interface

uses
  Windows,
  Direct3D9, DXUtil;

const
  // Font creation flags
  D3DFONT_BOLD        = $0001;
  D3DFONT_ITALIC      = $0002;
  D3DFONT_ZENABLE     = $0004;

  // Font rendering flags
  D3DFONT_CENTERED_X  = $0001;
  D3DFONT_CENTERED_Y  = $0002;
  D3DFONT_TWOSIDED    = $0004;
  D3DFONT_FILTERED    = $0008;

type
  CD3DFont = class

    m_strFontName: array[0..79] of Char;  // Font properties
    m_dwFontHeight: DWORD;
    m_dwFontFlags: DWORD;

    m_pd3dDevice: IDirect3DDevice9;       // A D3DDevice used for rendering
    m_pTexture: IDirect3DTexture9;        // The d3d texture for this font
    m_pVB: IDirect3DVertexBuffer9;        // VertexBuffer for rendering text
    m_dwTexWidth: DWORD;                  // Texture dimensions
    m_dwTexHeight: DWORD;
    m_fTextScale: Single;
    m_fTexCoords: array[0..128-32-1, 0..3] of Single;
    m_dwSpacing: DWORD;                   // Character pixel spacing per side

    // Stateblocks for setting and restoring render states
    m_pStateBlockSaved: IDirect3DStateBlock9;
    m_pStateBlockDrawText: IDirect3DStateBlock9;

    function CreateGDIFont(hDC: Windows.HDC; out pFont: HFONT): HRESULT;
    function PaintAlphabet(hDC: Windows.HDC; bMeasureOnly: Boolean = False): HRESULT;

  public
    // 2D and 3D text drawing functions
    function DrawText(sx, sy: Single; dwColor: DWORD;
                      strText: PChar; dwFlags: DWORD = 0): HRESULT;
    function DrawTextScaled(x, y, z: Single; fXScale, fYScale: Single; dwColor: DWORD;
                            strText: PChar; dwFlags: DWORD = 0): HRESULT;
    function Render3DText(strText: PChar; dwFlags: DWORD = 0): HRESULT;

    // Function to get extent of text
    function GetTextExtent(strText: PChar; pSize: PSize): HRESULT;

    // Initializing and destroying device-dependent objects
    function InitDeviceObjects(pd3dDevice: IDirect3DDevice9): HRESULT;
    function RestoreDeviceObjects: HRESULT;
    function InvalidateDeviceObjects: HRESULT;
    function DeleteDeviceObjects: HRESULT;

    // Constructor / destructor
    //Todo: Do we really need VIRTUAL constructor
    constructor Create(const strFontName: PChar; dwHeight: DWORD; dwFlags: DWORD = 0); // virtual;
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils, Math, 
  D3DX9;

//-----------------------------------------------------------------------------
// Custom vertex types for rendering text
//-----------------------------------------------------------------------------
const
  MAX_NUM_VERTICES = 50*6;

type
  PFont2DVertex = ^TFont2DVertex;
  TFont2DVertex = packed record
    p: TD3DXVector4;
    color: DWORD;
    tu, tv: Single;
  end;

  PFont3DVertex = ^TFont3DVertex;
  TFont3DVertex = packed record
    p: TD3DXVector3;
    n: TD3DXVector3;
    tu, tv: Single;
  end;

const
  D3DFVF_FONT2DVERTEX = (D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1);
  D3DFVF_FONT3DVERTEX = (D3DFVF_XYZ or D3DFVF_NORMAL or D3DFVF_TEX1);

function InitFont2DVertex(const p: TD3DXVector4; color: TD3DColor;
  tu, tv: Single): TFont2DVertex;
var
  v: TFont2DVertex;
begin
  v.p := p;   v.color := color;   v.tu := tu;   v.tv := tv;
  Result:= v;
end;

function InitFont3DVertex(const p: TD3DXVector3; const n: TD3DXVector3;
  tu, tv: Single): TFont3DVertex;
var
  v: TFont3DVertex;
begin
  v.p := p;   v.n := n;   v.tu := tu;   v.tv := tv;
  Result:= v;
end;



//-----------------------------------------------------------------------------
// Name: CD3DFont()
// Desc: Font class constructor
//-----------------------------------------------------------------------------
constructor CD3DFont.Create(const strFontName: PChar; dwHeight, dwFlags: DWORD);
begin
  StrLCopy(m_strFontName, strFontName, SizeOf(m_strFontName) div SizeOf(Char));
  m_strFontName[SizeOf(m_strFontName) div SizeOf(Char) - 1]:= #0;
  m_dwFontHeight         := dwHeight;
  m_dwFontFlags          := dwFlags;
  m_dwSpacing            := 0;

  m_pd3dDevice           := nil;
  m_pTexture             := nil;
  m_pVB                  := nil;

  m_pStateBlockSaved     := nil;
  m_pStateBlockDrawText  := nil;
end;


//-----------------------------------------------------------------------------
// Name: ~CD3DFont()
// Desc: Font class destructor
//-----------------------------------------------------------------------------
destructor CD3DFont.Destroy;
begin
  InvalidateDeviceObjects;
  DeleteDeviceObjects;
  inherited Destroy;
end;


//-----------------------------------------------------------------------------
// Name: CreateGDIFont
// Desc: Create a font based on the current state of related member variables
//       and return the handle (or null on error)
//-----------------------------------------------------------------------------
function CD3DFont.CreateGDIFont(hDC: Windows.HDC; out pFont: HFONT): HRESULT;
var
  nHeight: Integer;
  dwBold: DWORD;
  dwItalic: DWORD;
begin
  // Create a font.  By specifying ANTIALIASED_QUALITY, we might get an
  // antialiased font, but this is not guaranteed.
  nHeight := -MulDiv(m_dwFontHeight,
                     Trunc(GetDeviceCaps(hDC, LOGPIXELSY) * m_fTextScale), //todo: round or trunc ???
                     72);
  if (m_dwFontFlags and D3DFONT_BOLD = D3DFONT_BOLD)
    then dwBold:= FW_BOLD
    else dwBold:= FW_NORMAL;
  if ((m_dwFontFlags and D3DFONT_ITALIC) = D3DFONT_ITALIC)
    then dwItalic:= 1
    else dwItalic:= 0;
  pFont := CreateFont(nHeight, 0, 0, 0, dwBold, dwItalic,
                      0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
                      CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY,
                      VARIABLE_PITCH, m_strFontName);

  if (pFont = 0) then Result:= E_FAIL else Result:= S_OK;
end;



//-----------------------------------------------------------------------------
// Name: PaintAlphabet
// Desc: Paint the printable characters for the given GDI font onto the
//       provided device context. If the bMeasureOnly flag is set, no drawing
//       will occur.
//-----------------------------------------------------------------------------
function CD3DFont.PaintAlphabet(hDC: Windows.HDC; bMeasureOnly: Boolean = False): HRESULT;
var
  str: array[0..1] of Char; // "x" -> One-character, null-terminated string
  size: TSize;
  x, y: DWORD;
  c: Char;
begin
  str := 'x'#0;

  // Calculate the spacing between characters based on line height
  if not GetTextExtentPoint32(hDC, str, 1, size) then
  begin
    Result:= E_FAIL;
    Exit;
  end;
  m_dwSpacing := Ceil(size.cy * 0.3);

  // Set the starting point for the drawing
  x := m_dwSpacing;
  y := 0;

  // For each character, draw text on the DC and advance the current position
  for c := #32 to #126 do
  begin
    str[0] := c;
    if not GetTextExtentPoint32(hDC, str, 1, size) then
    begin
      Result:= E_FAIL;
      Exit;
    end;

    if (x + DWORD(size.cx) + m_dwSpacing) > m_dwTexWidth then
    begin
      x:= m_dwSpacing;
      y:= y + DWORD(size.cy) + 1;
    end;

    // Check to see if there's room to write the character here
    if (y + DWORD(size.cy) > m_dwTexHeight) then
    begin
      Result:= D3DERR_MOREDATA;
      Exit;
    end;

    if not bMeasureOnly then
    begin
      // Perform the actual drawing
      if not ExtTextOut(hDC, x+0, y+0, ETO_OPAQUE, nil, str, 1, nil) then
      begin
        Result:= E_FAIL;
        Exit;
      end;

      m_fTexCoords[Ord(c)-32][0] := ((x + 0              - m_dwSpacing))/m_dwTexWidth;
      m_fTexCoords[Ord(c)-32][1] := ((y + 0              + 0          ))/m_dwTexHeight;
      m_fTexCoords[Ord(c)-32][2] := ((x + DWORD(size.cx) + m_dwSpacing))/m_dwTexWidth;
      m_fTexCoords[Ord(c)-32][3] := ((y + DWORD(size.cy) + 0          ))/m_dwTexHeight;
    end;

    x:= x + DWORD(size.cx) + (2 * m_dwSpacing);
  end;

  Result:= S_OK;
end;

//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initializes device-dependent objects, including the vertex buffer used
//       for rendering text and the texture map which stores the font image.
//-----------------------------------------------------------------------------
function CD3DFont.InitDeviceObjects(pd3dDevice: IDirect3DDevice9): HRESULT;
type
  pBit = array[0..0] of DWORD;
  PpBit = ^pBit;
var
  hFont, hFontOld: Windows.HFONT;
  hDC: Windows.HDC;
  hbmBitmap: HBITMAP;
  hbmOld: HGDIOBJ;
  d3dCaps: TD3DCaps9;
  bFirstRun: Boolean; // Flag clear after first run

  pBitmapBits: PpBit;
  bmi: TBitmapInfo;
  x, y: DWORD;
  d3dlr: TD3DLockedRect;
  pDstRow: PByte;
  pDst16: PWord;
  bAlpha: Byte; // 4-bit measure of pixel intensity
label
  LCleanReturn;
begin
  hFont := 0;
  hFontOld := 0;
  hbmBitmap := 0;
  hbmOld := 0;

  // Keep a local copy of the device
  m_pd3dDevice := pd3dDevice;

  // Assume we will draw fonts into texture without scaling unless the
  // required texture size is found to be larger than the device max
  m_fTextScale:= 1.0;

  hDC := CreateCompatibleDC(0);
  SetMapMode(hDC, MM_TEXT);

  Result:= CreateGDIFont(hDC, hFont);
  if FAILED(Result) then
    goto LCleanReturn;

  hFontOld := SelectObject(hDC, hFont);

  // Calculate the dimensions for the smallest power-of-two texture which
  // can hold all the printable characters
  m_dwTexHeight := 128; m_dwTexWidth := m_dwTexHeight;
  Result := PaintAlphabet(hDC, True);
  while (D3DERR_MOREDATA = Result) do
  begin
    m_dwTexWidth  := m_dwTexWidth * 2;
    m_dwTexHeight := m_dwTexHeight * 2;
    Result := PaintAlphabet(hDC, True);
  end;

  if FAILED(Result) then
    goto LCleanReturn;

  // If requested texture is too big, use a smaller texture and smaller font,
  // and scale up when rendering.
  m_pd3dDevice.GetDeviceCaps(d3dCaps);

  if (m_dwTexWidth > d3dCaps.MaxTextureWidth) then
  begin
    m_fTextScale := d3dCaps.MaxTextureWidth / m_dwTexWidth;
    m_dwTexHeight := d3dCaps.MaxTextureWidth;
    m_dwTexWidth := m_dwTexHeight;

    bFirstRun:= True; // Flag clear after first run

    repeat
      // If we've already tried fitting the new text, the scale is still 
      // too large. Reduce and try again.
      if not bFirstRun then
        m_fTextScale := m_fTextScale * 0.9;

      // The font has to be scaled to fit on the maximum texture size; our
      // current font is too big and needs to be recreated to scale.
      DeleteObject(SelectObject(hDC, hFontOld));

      Result := CreateGDIFont(hDC, hFont);
      if FAILED(Result) then
        goto LCleanReturn;

      hFontOld := SelectObject(hDC, hFont);

      bFirstRun := False;
    until False;
    repeat
      Result := PaintAlphabet(hDC, True);
    until (Result <> D3DERR_MOREDATA);
  end;


  // Create a new texture for the font
  Result := m_pd3dDevice.CreateTexture(m_dwTexWidth, m_dwTexHeight, 1,
                                       0, D3DFMT_A4R4G4B4,
                                       D3DPOOL_MANAGED, m_pTexture, nil);
  if FAILED(Result) then
    goto LCleanReturn;

  // Prepare to create a bitmap
  ZeroMemory(@(bmi.bmiHeader), SizeOf(TBitmapInfoHeader));
  bmi.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
  bmi.bmiHeader.biWidth       :=  m_dwTexWidth;
  bmi.bmiHeader.biHeight      := -m_dwTexHeight;
  bmi.bmiHeader.biPlanes      := 1;
  bmi.bmiHeader.biCompression := BI_RGB;
  bmi.bmiHeader.biBitCount    := 32;

  // Create a bitmap for the font
  hbmBitmap := CreateDIBSection(hDC, bmi, DIB_RGB_COLORS, Pointer(pBitmapBits), 0, 0);

  hbmOld := SelectObject(hDC, hbmBitmap);

  // Set text properties
  SetTextColor(hDC, RGB(255, 255, 255));
  SetBkColor  (hDC, $00000000);
  SetTextAlign(hDC, TA_TOP);

  // Paint the alphabet onto the selected bitmap
  Result:= PaintAlphabet(hDC, False);
  if FAILED(Result) then
    goto LCleanReturn;

  // Lock the surface and write the alpha values for the set pixels
  m_pTexture.LockRect(0, d3dlr, nil, 0);
  pDstRow := d3dlr.pBits;

  for y := 0 to (m_dwTexHeight - 1) do
  begin
    pDst16 := PWord(pDstRow);
    for x := 0 to (m_dwTexWidth - 1) do
    begin
      bAlpha := (pBitmapBits^[m_dwTexWidth*y + x] and $ff) shr 4;
      if (bAlpha > 0) then
      begin
        pDst16^ := Word((bAlpha shl 12) or $0fff);
        Inc(pDst16);
      end else
      begin
        pDst16^ := $0000;
        Inc(pDst16);
      end;
    end;
    pDstRow := PByte(Integer(pDstRow) + d3dlr.Pitch);
  end;

  Result:= S_OK;

  // Done updating texture, so clean up used objects
LCleanReturn:
  if (m_pTexture <> nil) then 
    m_pTexture.UnlockRect(0);

  SelectObject(hDC, hbmOld);
  SelectObject(hDC, hFontOld);
  DeleteObject(hbmBitmap);
  DeleteObject(hFont);
  DeleteDC(hDC);
end;


//-----------------------------------------------------------------------------
// Name: RestoreDeviceObjects()
// Desc:
//-----------------------------------------------------------------------------
function CD3DFont.RestoreDeviceObjects: HRESULT;
var
  vertexSize: Integer;
  which: LongWord;
begin
  // Create vertex buffer for the letters
  vertexSize := Max(SizeOf(TFont2DVertex), SizeOf(TFont2DVertex));
  Result:= m_pd3dDevice.CreateVertexBuffer(MAX_NUM_VERTICES * vertexSize,
                                           D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, 0,
                                           D3DPOOL_DEFAULT, m_pVB, nil);
  if FAILED(Result) then Exit;

  // Create the state blocks for rendering text
  for which:= 0 to 1 do
  begin
    m_pd3dDevice.BeginStateBlock;
    m_pd3dDevice.SetTexture(0, m_pTexture);

    if (D3DFONT_ZENABLE and m_dwFontFlags) <> 0 then
      m_pd3dDevice.SetRenderState(D3DRS_ZENABLE, iTrue)
    else
      m_pd3dDevice.SetRenderState(D3DRS_ZENABLE, iFalse);

    m_pd3dDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1);
    m_pd3dDevice.SetRenderState(D3DRS_SRCBLEND,         D3DBLEND_SRCALPHA);
    m_pd3dDevice.SetRenderState(D3DRS_DESTBLEND,        D3DBLEND_INVSRCALPHA);
    m_pd3dDevice.SetRenderState(D3DRS_ALPHATESTENABLE,  1);
    m_pd3dDevice.SetRenderState(D3DRS_ALPHAREF,         $08);
    m_pd3dDevice.SetRenderState(D3DRS_ALPHAFUNC,        D3DCMP_GREATEREQUAL);
    m_pd3dDevice.SetRenderState(D3DRS_FILLMODE,         D3DFILL_SOLID);
    m_pd3dDevice.SetRenderState(D3DRS_CULLMODE,         D3DCULL_CCW);
    m_pd3dDevice.SetRenderState(D3DRS_ZENABLE,          0);
    m_pd3dDevice.SetRenderState(D3DRS_STENCILENABLE,    0);
    m_pd3dDevice.SetRenderState(D3DRS_CLIPPING,         1);
    m_pd3dDevice.SetRenderState(D3DRS_CLIPPLANEENABLE,  0);
    m_pd3dDevice.SetRenderState(D3DRS_VERTEXBLEND,      D3DVBF_DISABLE);
    m_pd3dDevice.SetRenderState(D3DRS_INDEXEDVERTEXBLENDENABLE, 0);
    m_pd3dDevice.SetRenderState(D3DRS_FOGENABLE,        0);
    m_pd3dDevice.SetRenderState(D3DRS_COLORWRITEENABLE,
        D3DCOLORWRITEENABLE_RED  or D3DCOLORWRITEENABLE_GREEN or
        D3DCOLORWRITEENABLE_BLUE or D3DCOLORWRITEENABLE_ALPHA);

    m_pd3dDevice.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_MODULATE);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_TEXCOORDINDEX, 0);
    m_pd3dDevice.SetTextureStageState(0, D3DTSS_TEXTURETRANSFORMFLAGS, D3DTTFF_DISABLE);
    m_pd3dDevice.SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_DISABLE);
    m_pd3dDevice.SetTextureStageState(1, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_POINT);
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);

    if (which = 0) then
      m_pd3dDevice.EndStateBlock(m_pStateBlockSaved)
    else
      m_pd3dDevice.EndStateBlock(m_pStateBlockDrawText);
  end;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name: InvalidateDeviceObjects()
// Desc: Destroys all device-dependent objects
//-----------------------------------------------------------------------------
function CD3DFont.InvalidateDeviceObjects: HRESULT;
begin
  SAFE_RELEASE(m_pVB);
  SAFE_RELEASE(m_pStateBlockSaved);
  SAFE_RELEASE(m_pStateBlockDrawText);

  Result:= S_OK;
end;




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Destroys all device-dependent objects
//-----------------------------------------------------------------------------
function CD3DFont.DeleteDeviceObjects: HRESULT;
begin
  SAFE_RELEASE(m_pTexture);
  m_pd3dDevice:= nil; //BAA: no Release in original C++
  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name: GetTextExtent()
// Desc: Get the dimensions of a text string
//-----------------------------------------------------------------------------
function CD3DFont.GetTextExtent(strText: PChar; pSize: PSize): HRESULT;
var
  fRowWidth, fRowHeight, fWidth, fHeight: Single;
  c: Char;
  tx1, tx2: Single;
begin
  if (nil = strText) or (nil = pSize) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  fRowWidth := 0.0;
  fRowHeight:= (m_fTexCoords[0][3]-m_fTexCoords[0][1])*m_dwTexHeight;
  fWidth    := 0.0;
  fHeight   := fRowHeight;

  while (strText^ <> #0) do
  begin
    c := strText^;
    Inc(strText);

    if (c = #10) then
    begin
      fRowWidth := 0.0;
      fHeight  := fHeight + fRowHeight;
    end;

    if ((Ord(c)-32) < 0) or (Ord(c)-32 >= 128-32) then Continue;

    tx1 := m_fTexCoords[Byte(c)-32][0];
    tx2 := m_fTexCoords[Byte(c)-32][2];

    fRowWidth := fRowWidth + (tx2-tx1)*m_dwTexWidth - 2*m_dwSpacing;

    if (fRowWidth > fWidth) then fWidth := fRowWidth;
  end;

  pSize^.cx := Trunc(fWidth);
  pSize^.cy := Trunc(fHeight);

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name: DrawTextScaled()
// Desc: Draws scaled 2D text.  Note that x and y are in viewport coordinates
//       (ranging from -1 to +1).  fXScale and fYScale are the size fraction
//       relative to the entire viewport.  For example, a fXScale of 0.25 is
//       1/8th of the screen width.  This allows you to output text at a fixed
//       fraction of the viewport, even if the screen or window size changes.
//-----------------------------------------------------------------------------
function CD3DFont.DrawTextScaled(x, y, z: Single; fXScale, fYScale: Single;
                                 dwColor: DWORD; strText: PChar; dwFlags: DWORD): HRESULT;
var
  vp: TD3DViewport9;
  fLineHeight, xFinal, sx, sy, sz, rhw, fStartX: Single;
  strTextTmp: PChar;
  c: Char;
  tx1, tx2: Single;
  w: Single;

  pVertices: PFont2DVertex;
  dwNumTriangles: DWORD;
  ty1, ty2: Single;
  h: Single;
begin
  if (m_pd3dDevice = nil) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // Set up renderstate
  m_pStateBlockSaved.Capture;
  m_pStateBlockDrawText.Apply;
  m_pd3dDevice.SetFVF(D3DFVF_FONT2DVERTEX);
  m_pd3dDevice.SetPixelShader(nil);
  m_pd3dDevice.SetStreamSource(0, m_pVB, 0, SizeOf(TFont2DVertex));

  // Set filter states
  if (dwFlags and D3DFONT_FILTERED = D3DFONT_FILTERED) then
  begin
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
  end;

  m_pd3dDevice.GetViewport(vp);
  fLineHeight := ( m_fTexCoords[0][3] - m_fTexCoords[0][1] ) * m_dwTexHeight;

  // Center the text block in the viewport
  if (dwFlags and D3DFONT_CENTERED_X = D3DFONT_CENTERED_X) then
  begin
    strTextTmp := strText;
    xFinal := 0.0;

    while (strTextTmp^ <> #0) do
    begin
      c := strTextTmp^; Inc(strTextTmp);

      if (c = #10) then Break;  // Isn't supported.
      if ((Ord(c)-32) < 0) or (Ord(c)-32 >= 128-32) then Continue;

      tx1 := m_fTexCoords[Ord(c)-32][0];
      tx2 := m_fTexCoords[Ord(c)-32][2];

      w := (tx2-tx1)*m_dwTexWidth;

      w := w * (fXScale*vp.Height)/fLineHeight;

      xFinal:= xFinal + w - (2 * m_dwSpacing) * (fXScale*vp.Height)/fLineHeight;
    end;

    x := -xFinal/vp.Width;
  end;
  if (dwFlags and D3DFONT_CENTERED_Y = D3DFONT_CENTERED_Y) then
  begin
    y := -fLineHeight/vp.Height;
  end;

  sx  := (x+1.0)*vp.Width/2;
  sy  := (y+1.0)*vp.Height/2;
  sz  := z;
  rhw := 1.0;

  // Adjust for character spacing
  sx := sx - m_dwSpacing * (fXScale*vp.Height)/fLineHeight;
  fStartX := sx;

  // Fill vertex buffer
  dwNumTriangles := 0;
  m_pVB.Lock(0, 0, Pointer(pVertices), D3DLOCK_DISCARD);

  while (strText <> nil) do
  begin
    c := strText^;
    Inc(strText);

    if (c = #10) then
    begin
      sx := fStartX;
      sy := sy + fYScale*vp.Height;
    end;
    if ((Ord(c)-32) < 0) or (Ord(c)-32 >= 128-32) then Continue;

    tx1 := m_fTexCoords[Ord(c)-32][0];
    ty1 := m_fTexCoords[Ord(c)-32][1];
    tx2 := m_fTexCoords[Ord(c)-32][2];
    ty2 := m_fTexCoords[Ord(c)-32][3];

    w := (tx2-tx1)*m_dwTexWidth;
    h := (ty2-ty1)*m_dwTexHeight;

    w := w*(fXScale*vp.Height)/fLineHeight;
    h := h*(fYScale*vp.Height)/fLineHeight;

    if (c <> ' ') then
    begin
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+0-0.5, sy+h-0.5, sz, rhw), dwColor, tx1, ty2); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+0-0.5, sy+0-0.5, sz, rhw), dwColor, tx1, ty1); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+w-0.5, sy+h-0.5, sz, rhw), dwColor, tx2, ty2); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+w-0.5, sy+0-0.5, sz, rhw), dwColor, tx2, ty1); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+w-0.5, sy+h-0.5, sz, rhw), dwColor, tx2, ty2); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+0-0.5, sy+0-0.5, sz, rhw), dwColor, tx1, ty1); Inc(pVertices);
      dwNumTriangles := dwNumTriangles + 2;

      if (dwNumTriangles*3 > (MAX_NUM_VERTICES-6)) then
      begin
        // Unlock, render, and relock the vertex buffer
        m_pVB.Unlock;
        m_pd3dDevice.DrawPrimitive(D3DPT_TRIANGLELIST, 0, dwNumTriangles);
        m_pVB.Lock(0, 0, Pointer(pVertices), D3DLOCK_DISCARD);
        dwNumTriangles := 0;
      end;
    end;

    sx := sx + w - (2 * m_dwSpacing) * (fXScale*vp.Height)/fLineHeight;
  end;

  // Unlock and render the vertex buffer
  m_pVB.Unlock;
  if (dwNumTriangles > 0) then
      m_pd3dDevice.DrawPrimitive(D3DPT_TRIANGLELIST, 0, dwNumTriangles);

  // Restore the modified renderstates
  m_pStateBlockSaved.Apply;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name: DrawText()
// Desc: Draws 2D text. Note that sx and sy are in pixels
//-----------------------------------------------------------------------------
function CD3DFont.DrawText(sx, sy: Single; dwColor: DWORD;
  strText: PChar; dwFlags: DWORD): HRESULT;
var
  vp: TD3DViewport9;
  strTextTmp: PChar;
  xFinal: Single;
  fLineHeight: Single;

  fStartX: Single;
  pVertices: PFont2DVertex;
  dwNumTriangles: DWORD;
  c: Char;
  tx1, ty1, tx2, ty2: Single;
  w, h: Single;
begin
  if (m_pd3dDevice = nil) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // Setup renderstate
  m_pStateBlockSaved.Capture;
  m_pStateBlockDrawText.Apply;
  m_pd3dDevice.SetFVF(D3DFVF_FONT2DVERTEX);
  m_pd3dDevice.SetPixelShader(nil);
  m_pd3dDevice.SetStreamSource(0, m_pVB, 0, SizeOf(TFont2DVertex));

  // Set filter states
  if (dwFlags and D3DFONT_FILTERED = D3DFONT_FILTERED) then
  begin
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
  end;

  // Center the text block in the viewport
  if (dwFlags and D3DFONT_CENTERED_X = D3DFONT_CENTERED_X) then
  begin
    m_pd3dDevice.GetViewport(vp);
    strTextTmp := strText;
    xFinal := 0.0;

    while (strTextTmp^ <> #0) do
    begin
      c := strTextTmp^;
      Inc(strTextTmp);

      if (c = #10) then Break;  // Isn't supported.
      if ((Ord(c)-32) < 0) or (Ord(c)-32 >= 128-32) then Continue;

      tx1 := m_fTexCoords[Ord(c)-32][0];
      tx2 := m_fTexCoords[Ord(c)-32][2];

      w := (tx2-tx1) *  m_dwTexWidth / m_fTextScale;

      xFinal := xFinal + w - (2 * m_dwSpacing);
    end;

    sx := (vp.Width-xFinal)/2.0;
  end;
  if (dwFlags and D3DFONT_CENTERED_Y = D3DFONT_CENTERED_Y) then
  begin
    m_pd3dDevice.GetViewport(vp);
    fLineHeight := ((m_fTexCoords[0][3]-m_fTexCoords[0][1])*m_dwTexHeight);
    sy := (vp.Height - fLineHeight)/2;
  end;

  // Adjust for character spacing
  sx := sx - m_dwSpacing;
  fStartX := sx;

  // Fill vertex buffer
  pVertices:= nil;
  dwNumTriangles := 0;
  m_pVB.Lock(0, 0, Pointer(pVertices), D3DLOCK_DISCARD);

  while (strText^ <> #0) do
  begin
    c := strText^;
    Inc(strText);

    if (c = #10) then
    begin
      sx := fStartX;
      sy := sy + (m_fTexCoords[0][3]-m_fTexCoords[0][1])*m_dwTexHeight;
    end;
    if ((Ord(c)-32) < 0) or (Ord(c)-32 >= 128-32) then Continue;

    tx1 := m_fTexCoords[byte(c)-32][0];
    ty1 := m_fTexCoords[byte(c)-32][1];
    tx2 := m_fTexCoords[byte(c)-32][2];
    ty2 := m_fTexCoords[byte(c)-32][3];

    w := (tx2-tx1) * m_dwTexWidth  / m_fTextScale;
    h := (ty2-ty1) * m_dwTexHeight / m_fTextScale;

    if (c <> ' ') then
    begin
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+0-0.5,sy+h-0.5,0.9,1.0), dwColor, tx1, ty2); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+0-0.5,sy+0-0.5,0.9,1.0), dwColor, tx1, ty1); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+w-0.5,sy+h-0.5,0.9,1.0), dwColor, tx2, ty2); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+w-0.5,sy+0-0.5,0.9,1.0), dwColor, tx2, ty1); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+w-0.5,sy+h-0.5,0.9,1.0), dwColor, tx2, ty2); Inc(pVertices);
      pVertices^ := InitFont2DVertex(D3DXVector4(sx+0-0.5,sy+0-0.5,0.9,1.0), dwColor, tx1, ty1); Inc(pVertices);
      dwNumTriangles := dwNumTriangles + 2;

      if (dwNumTriangles*3 > MAX_NUM_VERTICES - 6)  then
      begin
        // Unlock, render, and relock the vertex buffer
        m_pVB.Unlock;
        m_pd3dDevice.DrawPrimitive(D3DPT_TRIANGLELIST, 0, dwNumTriangles);
        pVertices:= nil;
        m_pVB.Lock(0, 0, Pointer(pVertices), D3DLOCK_DISCARD);
        dwNumTriangles := 0;
      end;
    end;

    sx := sx + w - (2 * m_dwSpacing);
  end;

  // Unlock and render the vertex buffer
  m_pVB.Unlock;
  if (dwNumTriangles > 0) then
      m_pd3dDevice.DrawPrimitive(D3DPT_TRIANGLELIST, 0, dwNumTriangles);

  // Restore the modified renderstates
  m_pStateBlockSaved.Apply;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name: Render3DText()
// Desc: Renders 3D text
//-----------------------------------------------------------------------------
function CD3DFont.Render3DText(strText: PChar; dwFlags: DWORD): HRESULT;
var
  x, y: Single;
  sz: TSize;
  fStartX: Single;
  c: Char;
  pVertices: PFont3DVertex;
  // dwVertex: DWORD; -  it's not used anyway
  dwNumTriangles: DWORD;
  tx1, ty1, tx2, ty2: Single;
  w, h: Single;
begin
  if (m_pd3dDevice = nil) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // Setup renderstate
  m_pStateBlockSaved.Capture;
  m_pStateBlockDrawText.Apply;
  m_pd3dDevice.SetFVF(D3DFVF_FONT3DVERTEX);
  m_pd3dDevice.SetPixelShader(nil);
  m_pd3dDevice.SetStreamSource(0, m_pVB, 0, SizeOf(TFont3DVertex));

  // Set filter states
  if (dwFlags and D3DFONT_FILTERED) = D3DFONT_FILTERED then
  begin
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    m_pd3dDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
  end;

  // Position for each text element
  x := 0.0;
  y := 0.0;

  // Center the text block at the origin (not the viewport)
  if (dwFlags and D3DFONT_CENTERED_X = D3DFONT_CENTERED_X) then
  begin
    GetTextExtent(strText, @sz);
    x := -((sz.cx)/10.0)/2.0;
    y := -((sz.cy)/10.0)/2.0;
  end;
  if (dwFlags and D3DFONT_CENTERED_Y = D3DFONT_CENTERED_Y) then
  begin
    GetTextExtent(strText, @sz);
    y := -(sz.cy/10.0)/2.0;
  end;

  // Turn off culling for two-sided text
  if (dwFlags and D3DFONT_TWOSIDED) = D3DFONT_TWOSIDED then
    m_pd3dDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

  // Adjust for character spacing
  x := x - m_dwSpacing / 10.0;
  fStartX := x;

  // Fill vertex buffer
  dwNumTriangles := 0;
  m_pVB.Lock(0, 0, Pointer(pVertices), D3DLOCK_DISCARD);

  c:= strText^;
  while (c <> #0) do
  begin
    Inc(strText);
    if (c = #10) then
    begin
      x := fStartX;
      y := y-(m_fTexCoords[0][3]-m_fTexCoords[0][1])*m_dwTexHeight/10.0;
    end;
    if ((Ord(c)-32) < 0) or (Ord(c)-32 >= 128-32) then Continue;

    tx1 := m_fTexCoords[Ord(c)-32][0];
    ty1 := m_fTexCoords[Ord(c)-32][1];
    tx2 := m_fTexCoords[Ord(c)-32][2];
    ty2 := m_fTexCoords[Ord(c)-32][3];

    w := (tx2-tx1) * m_dwTexWidth  / (10.0 * m_fTextScale);
    h := (ty2-ty1) * m_dwTexHeight / (10.0 * m_fTextScale);

    if (c <> ' ') then
    begin
      pVertices^ := InitFont3DVertex(D3DXVector3(x+0,y+0,0), D3DXVector3(0,0,-1), tx1, ty2); Inc(pVertices);
      pVertices^ := InitFont3DVertex(D3DXVector3(x+0,y+h,0), D3DXVector3(0,0,-1), tx1, ty1); Inc(pVertices);
      pVertices^ := InitFont3DVertex(D3DXVector3(x+w,y+0,0), D3DXVector3(0,0,-1), tx2, ty2); Inc(pVertices);
      pVertices^ := InitFont3DVertex(D3DXVector3(x+w,y+h,0), D3DXVector3(0,0,-1), tx2, ty1); Inc(pVertices);
      pVertices^ := InitFont3DVertex(D3DXVector3(x+w,y+0,0), D3DXVector3(0,0,-1), tx2, ty2); Inc(pVertices);
      pVertices^ := InitFont3DVertex(D3DXVector3(x+0,y+h,0), D3DXVector3(0,0,-1), tx1, ty1); Inc(pVertices);
      dwNumTriangles := dwNumTriangles + 2;

      if (dwNumTriangles*3 > (MAX_NUM_VERTICES - 6)) then
      begin
        // Unlock, render, and relock the vertex buffer
        m_pVB.Unlock;
        m_pd3dDevice.DrawPrimitive(D3DPT_TRIANGLELIST, 0, dwNumTriangles);
        m_pVB.Lock(0, 0, Pointer(pVertices), D3DLOCK_DISCARD);
        dwNumTriangles := 0;
      end;
    end;

    x := x + w;       
    c:= strText^;
  end;

  // Unlock and render the vertex buffer
  m_pVB.Unlock;
  if (dwNumTriangles > 0) then
    m_pd3dDevice.DrawPrimitive(D3DPT_TRIANGLELIST, 0, dwNumTriangles);

  // Restore the modified renderstates
  m_pStateBlockSaved.Apply;

  Result:= S_OK;
end;

end.
