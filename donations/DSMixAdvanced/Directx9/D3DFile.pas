(*----------------------------------------------------------------------------*
 *  DirectX 9 C++ common framework adaptation for Delphi by Alexey Barkovoy   *
 *  E-Mail: clootie@reactor.ru                                                *
 *                                                                            *
 *  Desc: Direct3D part of framework.                                         *
 *  Delphi versions 5-7 are supported                                         *
 *                                                                            *
 *  Modified: 11-Feb-2003                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://clootie.narod.ru/delphi                                         *
 *----------------------------------------------------------------------------*)
//-----------------------------------------------------------------------------
// File: D3DFile.h
//
// Desc: Support code for loading DirectX .X files.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
unit D3DFile;

{$I DirectX.inc}

interface

uses
  Windows,
  Direct3D9, D3DX9, DXFile, DXUtil;

type
  AD3DXMATERIAL = array [0..0] of TD3DXMaterial;
  PAD3DXMATERIAL = ^AD3DXMATERIAL;

  AD3DMATERIAL9 = array [0..0] of TD3DMaterial9;
  PAD3DMATERIAL9 = ^AD3DMATERIAL9;

  PIDirect3DTexture9 = ^IDirect3DTexture9;

  PAIDirect3DTexture9 = ^AIDirect3DTexture9;
  PPAIDirect3DTexture9 = ^_AIDirect3DTexture9;
  AIDirect3DTexture9 = array[0..0] of IDirect3DTexture9;
  _AIDirect3DTexture9 = array[0..0] of PIDirect3DTexture9;

//-----------------------------------------------------------------------------
// Name: class CD3DMesh
// Desc: Class for loading and rendering file-based meshes
//-----------------------------------------------------------------------------
type
  CD3DMesh = class;

  //BOOL (*EnumMeshCB)(CD3DMesh*,VOID*),
  EnumMeshCB = function (d3dmesh: CD3DMesh; p: Pointer): BOOL;

  CD3DMesh = class
  public
    m_strName: array[0..511] of Char;

    m_pSysMemMesh: ID3DXMesh;    // SysMem mesh, lives through resize
    m_pLocalMesh: ID3DXMesh;     // Local mesh, rebuilt on resize

    m_dwNumMaterials: DWORD; // Materials for the mesh
    m_pMaterials: PAD3DMATERIAL9;
    m_pTextures: PPAIDirect3DTexture9;
    m_bUseMaterials: BOOL;

  public
    // Rendering
    function Render(pd3dDevice: IDirect3DDevice9;
                    bDrawOpaqueSubsets: Boolean = True;
                    bDrawAlphaSubsets: Boolean = True): HRESULT;

    // Mesh access
    function GetSysMemMesh: ID3DXMesh; { return m_pSysMemMesh; }
    function GetLocalMesh: ID3DXMesh;  { return m_pLocalMesh; }

    // Rendering options
    procedure UseMeshMaterials(bFlag: Boolean); { m_bUseMaterials = bFlag; }
    function SetFVF(pd3dDevice: IDirect3DDevice9; dwFVF: DWORD): HRESULT;

    // Initializing
    function RestoreDeviceObjects(pd3dDevice: IDirect3DDevice9): HRESULT;
    function InvalidateDeviceObjects: HRESULT;

    // Creation/destruction
    function Create_(pd3dDevice: IDirect3DDevice9; const strFilename: PChar): HRESULT; overload;
    function Create_(pd3dDevice: IDirect3DDevice9; pFileData: IDirectXFileData): HRESULT; overload;
    function Destroy_: HRESULT;

    constructor Create; overload;
    constructor Create(strName: PChar); overload;
    destructor Destroy; override;
  end;



  //-----------------------------------------------------------------------------
  // Name: class CD3DFrame
  // Desc: Class for loading and rendering file-based meshes
  //-----------------------------------------------------------------------------
  CD3DFrame = class
  public
    m_strName: array[0..511] of Char;
    m_mat: TD3DXMATRIX;
    m_pMesh: CD3DMesh;

    m_pNext: CD3DFrame;
    m_pChild: CD3DFrame;

  public
    // Matrix access
    procedure SetMatrix(pmat: PD3DXMatrix); { m_mat = *pmat; }
    function  GetMatrix: PD3DXMatrix;        { return &m_mat; }

    function FindMesh(strMeshName: PChar): CD3DMesh;
    function FindFrame(strFrameName: PChar): CD3DFrame;
    function EnumMeshes(EnumMeshF: EnumMeshCB; pContext: Pointer): BOOL;

    function Destroy_: HRESULT;
    function RestoreDeviceObjects(pd3dDevice: IDirect3DDevice9): HRESULT;
    function InvalidateDeviceObjects: HRESULT;
    function Render(pd3dDevice: IDirect3DDevice9;
                    bDrawOpaqueSubsets: Boolean = True;
                    bDrawAlphaSubsets: Boolean = True;
                    pmatWorldMatrix: PD3DXMatrix = nil
                   ): HResult;

    constructor Create(strName: PChar);
    destructor Destroy; override;
  end;




//-----------------------------------------------------------------------------
// Name: class CD3DFile
// Desc: Class for loading and rendering file-based meshes
//-----------------------------------------------------------------------------
  CD3DFile = class(CD3DFrame)
    function LoadMesh(pd3dDevice: IDirect3DDevice9; pFileData: IDirectXFileData;
                      pParentFrame: CD3DFrame): HRESULT;
    function LoadFrame(pd3dDevice: IDirect3DDevice9; pFileData: IDirectXFileData;
                       pParentFrame: CD3DFrame): HRESULT;
  public
    function CreateFromFile(pd3dDevice: IDirect3DDevice9; strFilename: PChar): HRESULT; //was: "Create" in C++
    function CreateFromResource(pd3dDevice: IDirect3DDevice9; strResource, strType: PChar): HRESULT;
    // For pure devices, specify the world transform. If the world transform is not
    // specified on pure devices, this function will fail.
    function Render(pd3dDevice: IDirect3DDevice9;
      pmatWorldMatrix: PD3DXMatrix = nil): HRESULT;

    constructor Create;
  end;


implementation

uses
  SysUtils;

function CD3DMesh.GetSysMemMesh: ID3DXMesh;
begin
  Result:= m_pSysMemMesh;
end;

function CD3DMesh.GetLocalMesh: ID3DXMesh;
begin
  Result:= m_pLocalMesh;
end;

procedure CD3DMesh.UseMeshMaterials(bFlag: Boolean);
begin
  m_bUseMaterials:= bFlag;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------

constructor CD3DMesh.Create;
const //Clootie: Declaring const as optimal hack for Delphi5 and TMT4 compatibility
  DefaultName = 'CD3DFile_Mesh';
begin
  Create(DefaultName);
end;

//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------

constructor CD3DMesh.Create(strName: PChar);
begin
  if (strName = nil) then strName:= 'CD3DFile_Mesh';
  StrLCopy(m_strName, strName, SizeOf(m_strName) div SizeOf(Char));
  m_strName[SizeOf(m_strName) div SizeOf(Char) - 1] := #0;
  m_pSysMemMesh        := nil;
  m_pLocalMesh         := nil;
  m_dwNumMaterials     := 0;
  m_pMaterials         := nil;
  m_pTextures          := nil;
  m_bUseMaterials      := True;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
destructor CD3DMesh.Destroy;
begin
  Destroy_;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.Create_(pd3dDevice: IDirect3DDevice9; const strFilename: PChar): HRESULT;
var
  strPath: array[0..MAX_PATH-1] of Char;
  pAdjacencyBuffer: ID3DXBuffer;
  pMtrlBuffer: ID3DXBuffer;
  d3dxMtrls: PAD3DXMATERIAL;
  i:dword;
  strTexture: array[0..MAX_PATH-1] of Char;
  strTextureTemp: array[0..MAX_PATH-1] of Char;
label
  LEnd;
begin
  pAdjacencyBuffer:= nil;
  pMtrlBuffer:= nil;

  // Find the path for the file, and convert it to ANSI (for the D3DX API)
  DXUtil_FindMediaFileCb(strPath, SizeOf(strPath), strFilename);

  // Load the mesh
  Result:= D3DXLoadMeshFromX(strPath, D3DXMESH_SYSTEMMEM, pd3dDevice,
                             @pAdjacencyBuffer, @pMtrlBuffer, nil,
                             @m_dwNumMaterials, m_pSysMemMesh);
  if FAILED(Result) then Exit;

  // Optimize the mesh for performance
  Result:= m_pSysMemMesh.OptimizeInplace(
    D3DXMESHOPT_COMPACT or D3DXMESHOPT_ATTRSORT or D3DXMESHOPT_VERTEXCACHE,
    pAdjacencyBuffer.GetBufferPointer, nil, nil, nil);
  if FAILED(Result) then
  begin
    SAFE_RELEASE(pAdjacencyBuffer);
    SAFE_RELEASE(pMtrlBuffer);
    Exit;
  end;

  // Get material info for the mesh
  // Get the array of materials out of the buffer
  if (pMtrlBuffer <> nil) and (m_dwNumMaterials <> 0) then
  begin
    // Allocate memory for the materials and textures
    d3dxMtrls := pMtrlBuffer.GetBufferPointer;
    try
      GetMem(m_pMaterials, SizeOf(TD3DMaterial9)*m_dwNumMaterials);
      GetMem(m_pTextures, SizeOf(IDirect3DTexture9)*m_dwNumMaterials);
    except
      on EOutOfMemory do
      begin
        SAFE_RELEASE(pAdjacencyBuffer);
        SAFE_RELEASE(pMtrlBuffer);
        Result:= E_OUTOFMEMORY;
        Exit;
      end;
    else
      raise;
    end;

    // Copy each material and create it's texture
    if (m_dwNumMaterials <> 0) then for i:= 0 to (m_dwNumMaterials - 1) do
    begin
      // Copy the material
      m_pMaterials^[i] := d3dxMtrls^[i].MatD3D;
      m_pTextures^[i]  := nil;

      // Create a texture
      if (d3dxMtrls^[i].pTextureFilename <> nil) then
      begin
        DXUtil_ConvertAnsiStringToGenericCb(strTextureTemp, d3dxMtrls^[i].pTextureFilename, SizeOf(strTextureTemp));
        DXUtil_FindMediaFileCb(strTexture, SizeOf(strTexture), strTextureTemp);

        if (FAILED(D3DXCreateTextureFromFile(pd3dDevice, strTexture,
                                             IDirect3DTexture9(m_pTextures^[i]))))
        then m_pTextures^[i]:= nil;
      end;
    end;
  end;
  Result:= S_OK;

LEnd:
  SAFE_RELEASE(pAdjacencyBuffer);
  SAFE_RELEASE(pMtrlBuffer);
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.Create_(pd3dDevice: IDirect3DDevice9;
                          pFileData: IDirectXFileData): HRESULT;
var
  pMtrlBuffer: ID3DXBuffer;
  pAdjacencyBuffer: ID3DXBuffer;
  d3dxMtrls: PAD3DXMATERIAL;
  i: DWORD;
  strTexture: array[0..MAX_PATH-1] of Char;
  strTextureTemp: array[0..MAX_PATH-1] of Char;
begin
  pMtrlBuffer := nil;

  // Load the mesh from the DXFILEDATA object
  Result:= D3DXLoadMeshFromXof(pFileData, D3DXMESH_SYSTEMMEM, pd3dDevice,
                               @pAdjacencyBuffer, @pMtrlBuffer, nil,
                               @m_dwNumMaterials, m_pSysMemMesh);
  if FAILED(Result) then Exit;

  // Optimize the mesh for performance
  Result:= m_pSysMemMesh.OptimizeInplace(
    D3DXMESHOPT_COMPACT or D3DXMESHOPT_ATTRSORT or D3DXMESHOPT_VERTEXCACHE,
    pAdjacencyBuffer.GetBufferPointer, nil, nil, nil);
  if FAILED(Result) then
  begin
    SAFE_RELEASE(pAdjacencyBuffer);
    SAFE_RELEASE(pMtrlBuffer);
    Exit;
  end;

  // Get material info for the mesh
  // Get the array of materials out of the buffer
  if (pMtrlBuffer <> nil) and (m_dwNumMaterials <> 0) then
  begin
    // Allocate memory for the materials and textures
    d3dxMtrls := pMtrlBuffer.GetBufferPointer;
    try
      GetMem(m_pMaterials, SizeOf(TD3DMaterial9)*m_dwNumMaterials);
      GetMem(m_pTextures, SizeOf(IDirect3DTexture9)*m_dwNumMaterials);
    except
      on EOutOfMemory do
      begin
        SAFE_RELEASE(pAdjacencyBuffer);
        SAFE_RELEASE(pMtrlBuffer);
        Result:= E_OUTOFMEMORY;
        Exit;
      end;
    else
      raise;
    end;

    // Copy each material and create it's texture
    for i:= 0 to (m_dwNumMaterials - 1) do
    begin
      // Copy the material
      m_pMaterials^[i] := d3dxMtrls^[i].MatD3D;
      m_pTextures^[i]  := nil;

      // Create a texture
      if (d3dxMtrls^[i].pTextureFilename <> nil) then
      begin
        DXUtil_ConvertAnsiStringToGenericCb(strTextureTemp, d3dxMtrls^[i].pTextureFilename, SizeOf(strTextureTemp));
        DXUtil_FindMediaFileCb(strTexture, SizeOf(strTexture), strTextureTemp);

        if FAILED(D3DXCreateTextureFromFile(pd3dDevice, strTexture,
                                            IDirect3DTexture9(m_pTextures^[i])))
        then m_pTextures^[i]:= nil;
      end;
    end;
  end;
  Result:= S_OK;

  SAFE_RELEASE(pAdjacencyBuffer);
  SAFE_RELEASE(pMtrlBuffer);
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.SetFVF(
  pd3dDevice: IDirect3DDevice9; dwFVF: DWORD): HRESULT;
var
  pTempSysMemMesh: ID3DXMesh;
  pTempLocalMesh: ID3DXMesh;
begin
  Result:= E_FAIL;

  pTempSysMemMesh := nil;
  pTempLocalMesh  := nil;

  if (m_pSysMemMesh <> nil) then
  begin
    if FAILED(m_pSysMemMesh.CloneMeshFVF(D3DXMESH_SYSTEMMEM, dwFVF,
                                         pd3dDevice, pTempSysMemMesh))
    then Exit;
  end;

  if (m_pLocalMesh <> nil) then
  begin
    if FAILED(m_pLocalMesh.CloneMeshFVF(0, dwFVF, pd3dDevice, pTempLocalMesh)) then
    begin
      SAFE_RELEASE(pTempSysMemMesh);
      Exit;
    end;
  end;

  SAFE_RELEASE(m_pSysMemMesh);
  SAFE_RELEASE(m_pLocalMesh);

  if (pTempSysMemMesh <> nil) then m_pSysMemMesh := pTempSysMemMesh;
  if (pTempLocalMesh  <> nil) then m_pLocalMesh  := pTempLocalMesh;

  // Compute normals in case the meshes have them
  if (m_pSysMemMesh <> nil) then
    D3DXComputeNormals(m_pSysMemMesh, nil);
  if (m_pLocalMesh <> nil) then
    D3DXComputeNormals(m_pLocalMesh, nil);

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.RestoreDeviceObjects(pd3dDevice: IDirect3DDevice9): HRESULT;
begin
  Result:= E_FAIL;

  if (m_pSysMemMesh = nil) then Exit;

  // Make a local memory version of the mesh. Note: because we are passing in
  // no flags, the default behavior is to clone into local memory.
  if FAILED(m_pSysMemMesh.CloneMeshFVF(0, m_pSysMemMesh.GetFVF,
                                       pd3dDevice, m_pLocalMesh))
  then Exit;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.InvalidateDeviceObjects: HRESULT;
begin
  SAFE_RELEASE(m_pLocalMesh);
  Result:= S_OK;
end;

// This procedure cannot be used in TMT pascal
procedure SAFE_DELETE_ARRAY(var p: Pointer);
begin
  if (p <> nil) then
  begin
    FreeMem(p);
    P:= nil;
  end;
end;

//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.Destroy_: HRESULT;
var
  i: Cardinal;
begin
  InvalidateDeviceObjects;
  if (m_dwNumMaterials <> 0) then
    for i:= 0 to (m_dwNumMaterials - 1) do
      if (m_pTextures <> nil) then SAFE_RELEASE(IUnknown(m_pTextures^[i]));

  SAFE_DELETE_ARRAY(Pointer(m_pTextures));
  SAFE_DELETE_ARRAY(Pointer(m_pMaterials));

  SAFE_RELEASE(m_pSysMemMesh);

  m_dwNumMaterials:= 0;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DMesh.Render(pd3dDevice: IDirect3DDevice9; bDrawOpaqueSubsets: Boolean;
  bDrawAlphaSubsets: Boolean): HRESULT;
var
  i: DWORD;
begin
  if (nil = m_pLocalMesh) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // First, draw the subsets without alpha
  if bDrawOpaqueSubsets then
  begin
    for i:= 0 to (m_dwNumMaterials - 1) do
    begin
      if m_bUseMaterials then
      begin
        if (m_pMaterials^[i].Diffuse.a < 1.0) then
          Continue;
        pd3dDevice.SetMaterial(m_pMaterials^[i]);
        pd3dDevice.SetTexture(0, IDirect3DBaseTexture9(m_pTextures^[i]));
      end;
      m_pLocalMesh.DrawSubset(i);
    end;
  end;

  // Then, draw the subsets with alpha
  if (bDrawAlphaSubsets and m_bUseMaterials) then
  begin
    for i:= 0 to m_dwNumMaterials - 1 do
    begin
      if (m_pMaterials^[i].Diffuse.a = 1.0) then
        Continue;

      // Set the material and texture
      pd3dDevice.SetMaterial(m_pMaterials^[i]);
      pd3dDevice.SetTexture(0, IDirect3DBaseTexture9(m_pTextures^[i]));
      m_pLocalMesh.DrawSubset(i);
    end;
  end;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
constructor CD3DFrame.Create(strName: PChar);
begin
  StrLCopy(m_strName, strName, SizeOf(m_strName) div SizeOf(Char));
  m_strName[SizeOf(m_strName) div SizeOf(Char) - 1] := #0;
  D3DXMatrixIdentity(m_mat);
  m_pMesh  := nil;

  m_pChild := nil;
  m_pNext  := nil;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
destructor CD3DFrame.Destroy;
begin
  SAFE_DELETE(m_pChild);
  SAFE_DELETE(m_pNext);
end;

procedure CD3DFrame.SetMatrix(pmat: PD3DXMatrix);
begin
  m_mat:= pmat^;
end;

function CD3DFrame.GetMatrix: PD3DXMatrix;
begin
  Result:= @m_mat;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.EnumMeshes(EnumMeshF: EnumMeshCB; pContext: Pointer): BOOL;
begin
  if (m_pMesh  <> nil) then EnumMeshF(Pointer(m_pMesh), pContext);
  if (m_pChild <> nil) then m_pChild.EnumMeshes(EnumMeshF, pContext);
  if (m_pNext  <> nil) then m_pNext.EnumMeshes(EnumMeshF, pContext);

  Result:= True;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.FindMesh(strMeshName: PChar): CD3DMesh;
var
  pMesh: CD3DMesh;
begin
  if (m_pMesh <> nil) then
    if StrIComp(m_pMesh.m_strName, strMeshName) = 0 then
    begin
      Result:= CD3DMesh(m_pMesh);
      Exit;
    end;

  if (m_pChild <> nil) then
  begin
    pMesh:= m_pChild.FindMesh(strMeshName);
    if (nil <> pMesh) then
    begin
      Result:= pMesh;
      Exit;
    end;
  end;

  if (m_pNext <> nil) then
  begin
    pMesh := m_pNext.FindMesh(strMeshName);
    if (nil <> pMesh) then
    begin
      Result:= pMesh;
      Exit;
    end;
  end;

  Result:= nil;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.FindFrame(strFrameName: PChar): CD3DFrame;
var
  pFrame: CD3DFrame;
begin
  if StrIComp(m_strName, strFrameName) = 0 then
  begin
    Result:= Self;
    Exit;
  end;

  if (m_pChild <> nil) then
  begin
    pFrame:= m_pChild.FindFrame(strFrameName);
    if (pFrame <> nil) then
    begin
      Result:= pFrame;
      Exit;
    end;
  end;

  if (m_pNext <> nil) then
  begin
    pFrame:= m_pNext.FindFrame(strFrameName);
    if (pFrame <> nil) then
    begin
      Result:= pFrame;
      Exit;
    end;
  end;

  Result:= nil;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.Destroy_: HRESULT;
begin
  if (m_pMesh <> nil)  then m_pMesh.Destroy_;
  if (m_pChild <> nil) then m_pChild.Destroy_;
  if (m_pNext <> nil)  then m_pNext.Destroy_;

  SAFE_DELETE(m_pMesh);
  SAFE_DELETE(m_pNext);
  SAFE_DELETE(m_pChild);

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.RestoreDeviceObjects(pd3dDevice: IDirect3DDevice9): HRESULT;
begin
  if (m_pMesh <> nil)  then m_pMesh.RestoreDeviceObjects(pd3dDevice);
  if (m_pChild <> nil) then m_pChild.RestoreDeviceObjects(pd3dDevice);
  if (m_pNext <> nil)  then m_pNext.RestoreDeviceObjects(pd3dDevice);
  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.InvalidateDeviceObjects: HRESULT;
begin
  if (m_pMesh <> nil)  then m_pMesh.InvalidateDeviceObjects;
  if (m_pChild <> nil) then m_pChild.InvalidateDeviceObjects;
  if (m_pNext <> nil)  then m_pNext.InvalidateDeviceObjects;
  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFrame.Render(pd3dDevice: IDirect3DDevice9;
                    bDrawOpaqueSubsets: Boolean = True;
                    bDrawAlphaSubsets: Boolean = True;
                    pmatWorldMatrix: PD3DXMatrix = nil
                   ): HResult;

var
  matSavedWorld, matWorld: TD3DXMatrixA16;
begin
  // For pure devices, specify the world transform. If the world transform is not
  // specified on pure devices, this function will fail.
  if (nil = pmatWorldMatrix) then
      pd3dDevice.GetTransform(D3DTS_WORLD, matSavedWorld)
  else
      matSavedWorld:= pmatWorldMatrix^;
  D3DXMatrixMultiply(matWorld, m_mat, matSavedWorld);
  pd3dDevice.SetTransform(D3DTS_WORLD, matWorld);

  if (m_pMesh <> nil) then
    m_pMesh.Render(pd3dDevice, bDrawOpaqueSubsets,
      bDrawAlphaSubsets);

  if (m_pChild <> nil) then
    m_pChild.Render(pd3dDevice, bDrawOpaqueSubsets,
      bDrawAlphaSubsets, @matWorld);

  pd3dDevice.SetTransform(D3DTS_WORLD, matSavedWorld);

  if (m_pNext <> nil) then
    m_pNext.Render(pd3dDevice, bDrawOpaqueSubsets,
      bDrawAlphaSubsets, @matSavedWorld);

  Result:= S_OK;
end;


function CmpGUID(const p1, p2: TGUID): Integer;
begin
  if CompareMem(@p1, @p2, SizeOf(TGUID)) then Result:= 0
  else Result:= 1;
end;

//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
constructor CD3DFile.Create;
begin
  inherited Create('CD3DFile_Root');
end;

//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFile.LoadFrame(
  pd3dDevice: IDirect3DDevice9; pFileData: IDirectXFileData;
  pParentFrame: CD3DFrame): HRESULT;
var
  pChildData: IDirectXFileData;
  pChildObj: IDirectXFileObject;
  //const GUID* pGUID;
  pGUID_: PGUID;
  cbSize: DWORD;
  pCurrentFrame: CD3DFrame;
  pmatMatrix: PD3DXMatrix;
  strAnsiName: array[0..511] of AnsiChar;
  strName: array[0..511] of Char;
  dwNameLength: DWORD;
begin
  pChildData := nil;
  pChildObj := nil;

  // Get the type of the object
  Result:= pFileData.GetType(pGUID_);
  if FAILED(Result) then Exit;

  if CmpGUID(pGUID_^, TID_D3DRMMesh) = 0 then
  begin
    Result:= LoadMesh(pd3dDevice, pFileData, pParentFrame);
    if FAILED(Result) then Exit;
  end;

  if CmpGUID(pGUID_^, TID_D3DRMFrameTransformMatrix) = 0 then
  begin
    Result:= pFileData.GetData(nil, cbSize, Pointer(pmatMatrix));
    if FAILED(Result) then Exit;

    // Update the parent's matrix with the new one
    pParentFrame.SetMatrix(pmatMatrix);
  end;

  if CmpGUID(pGUID_^, TID_D3DRMFrame) = 0 then
  begin
    // Get the frame name
    strAnsiName:= '';
    dwNameLength := 512;
    Result := pFileData.GetName(strAnsiName, dwNameLength);
    if FAILED(Result) then Exit;
    DXUtil_ConvertAnsiStringToGenericCb(strName, strAnsiName, SizeOf(strName));

    // Create the frame
    try
      pCurrentFrame := CD3DFrame.Create(strName);
    except
      on EOutOfMemory do
      begin
        Result:= E_OUTOFMEMORY;
        Exit;
      end;
    else
      raise;
    end;

    pCurrentFrame.m_pNext := pParentFrame.m_pChild;
    pParentFrame.m_pChild := pCurrentFrame;

    // Enumerate child objects
    while SUCCEEDED(pFileData.GetNextObject(pChildObj)) do
    begin
      // Query the child for its FileData
      Result:= pChildObj.QueryInterface(IID_IDirectXFileData, pChildData);
      if SUCCEEDED(Result) then
      begin
        Result:= LoadFrame(pd3dDevice, pChildData, pCurrentFrame);
        SAFE_RELEASE(pChildData);
      end;

      SAFE_RELEASE(pChildObj);

      if FAILED(Result) then Exit;
    end;
  end;

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFile.LoadMesh(
  pd3dDevice: IDirect3DDevice9; pFileData: IDirectXFileData;
  pParentFrame: CD3DFrame): HRESULT;
var
  strAnsiName: array[0..511] of AnsiChar;
  strName: array[0..MAX_PATH] of Char;
  dwNameLength: DWORD;
begin
  strAnsiName:= #0;
  // Currently only allowing one mesh per frame
  if (pParentFrame.m_pMesh <> nil) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // Get the mesh name
  strAnsiName := #0;
  dwNameLength := 512;
  Result:= pFileData.GetName(strAnsiName, dwNameLength);
  if FAILED(Result) then Exit;
  DXUtil_ConvertAnsiStringToGenericCb(strName, strAnsiName, SizeOf(strName));

  // Create the mesh
  try
    pParentFrame.m_pMesh:= CD3DMesh.Create(strName);
  except
    on EOutOfMemory do
    begin
      Result:= E_OUTOFMEMORY;
      Exit;
    end;
  else
    raise;
  end;
  pParentFrame.m_pMesh.Create_(pd3dDevice, pFileData);

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFile.CreateFromResource(pd3dDevice: IDirect3DDevice9; strResource, strType: PChar): HRESULT;
var
  pDXFile: IDirectXFile;
  pEnumObj: IDirectXFileEnumObject;
  pFileData: IDirectXFileData;
  strTypeAnsi: array[0..MAX_PATH-1] of Char;
  dxlr: TDXFileLoadResource;
begin
  pDXFile   := nil;
  pEnumObj  := nil;
  pFileData := nil;

  // Create a x file object
  Result:= DirectXFileCreate(pDXFile);
  if FAILED(Result) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // Register templates for d3drm and patch extensions.
  Result:= pDXFile.RegisterTemplates(@D3DRM_XTEMPLATES, D3DRM_XTEMPLATE_BYTES);
  if (FAILED(Result)) then
  begin
    SAFE_RELEASE(pDXFile);
    Result:= E_FAIL;
    Exit;
  end;

  DXUtil_ConvertGenericStringToAnsiCb(strTypeAnsi, strType, SizeOf(strTypeAnsi));

  dxlr.hModule := 0;
  dxlr.lpName := strResource;
  dxlr.lpType := strTypeAnsi;

  // Create enum object
  Result:= pDXFile.CreateEnumObject(@dxlr, DXFILELOAD_FROMRESOURCE, pEnumObj);
  if FAILED(Result) then
  begin
    SAFE_RELEASE(pDXFile);
    Exit;
  end;

  // Enumerate top level objects (which are always frames)
  while SUCCEEDED(pEnumObj.GetNextDataObject(pFileData)) do
  begin
    Result:= LoadFrame(pd3dDevice, pFileData, CD3DFrame(Self));
    SAFE_RELEASE(pFileData);
    if FAILED(Result) then
    begin
      SAFE_RELEASE(pEnumObj);
      SAFE_RELEASE(pDXFile);
      Result:= E_FAIL;
      Exit;
    end;
  end;

  SAFE_RELEASE(pFileData);
  SAFE_RELEASE(pEnumObj);
  SAFE_RELEASE(pDXFile);

  Result:= S_OK;
end;

//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFile.CreateFromFile(pd3dDevice: IDirect3DDevice9; strFilename: PChar): HRESULT;
var
  pDXFile: IDirectXFile;
  pEnumObj: IDirectXFileEnumObject;
  pFileData: IDirectXFileData;
  strPath: array[0..MAX_PATH-1] of Char;
  strPathANSI: array[0..MAX_PATH-1] of Char;
begin
  pDXFile   := nil;
  pEnumObj  := nil;
  pFileData := nil;

  // Create a x file object
  Result:= DirectXFileCreate(pDXFile);
  if FAILED(Result) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  // Register templates for d3drm and patch extensions.
  Result:= pDXFile.RegisterTemplates(@D3DRM_XTEMPLATES, D3DRM_XTEMPLATE_BYTES);
  if FAILED(Result) then
  begin
    SAFE_RELEASE(pDXFile);
    Result:= E_FAIL;
    Exit;
  end;

  // Find the path to the file, and convert it to ANSI (for the D3DXOF API)
  DXUtil_FindMediaFileCb(strPath, SizeOf(strPath), strFilename);
  DXUtil_ConvertGenericStringToAnsiCb(strPathANSI, strPath, SizeOf(strPathANSI));

  // Create enum object
  Result:= pDXFile.CreateEnumObject(@strPathANSI, DXFILELOAD_FROMFILE, pEnumObj);
  if FAILED(Result) then
  begin
    SAFE_RELEASE(pDXFile);
    Exit;
  end;

  // Enumerate top level objects (which are always frames)
  while SUCCEEDED(pEnumObj.GetNextDataObject(pFileData)) do
  begin
    Result:= LoadFrame(pd3dDevice, pFileData, CD3DFrame(Self));
    SAFE_RELEASE(pFileData);
    if FAILED(Result) then
    begin
      SAFE_RELEASE(pEnumObj);
      SAFE_RELEASE(pDXFile);
      Result:= E_FAIL;
      Exit;
    end;
  end;

  SAFE_RELEASE(pFileData);
  SAFE_RELEASE(pEnumObj);
  SAFE_RELEASE(pDXFile);

  Result:= S_OK;
end;


//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
function CD3DFile.Render(pd3dDevice: IDirect3DDevice9;
  pmatWorldMatrix: PD3DXMatrix = nil): HRESULT;
var
  matSavedWorld, matWorld: TD3DXMatrix;
begin
  // Set up the world transformation

  // For pure devices, specify the world transform. If the world transform is not
  // specified on pure devices, this function will fail.
  if (nil = pmatWorldMatrix) then
    pd3dDevice.GetTransform(D3DTS_WORLD, matSavedWorld)
  else
    matSavedWorld:= pmatWorldMatrix^;
  D3DXMatrixMultiply(matWorld, matSavedWorld, m_mat);
  pd3dDevice.SetTransform(D3DTS_WORLD, matWorld);

  // Render opaque subsets in the meshes
  if (m_pChild <> nil) then
    m_pChild.Render(pd3dDevice, TRUE, FALSE, @matWorld);

  // Enable alpha blending
  pd3dDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1);
  pd3dDevice.SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
  pd3dDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);

  // Render alpha subsets in the meshes
  if (m_pChild <> nil) then
    m_pChild.Render(pd3dDevice, FALSE, TRUE, @matWorld);

  // Restore state
  pd3dDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 0);
  pd3dDevice.SetTransform(D3DTS_WORLD, matSavedWorld);

  Result:= S_OK;
end;

end.

