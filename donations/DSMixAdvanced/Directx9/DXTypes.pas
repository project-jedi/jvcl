{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       extracted from various DirectX SDK include files              *}
{*                                                                            *}
{*  Content:    DirectX 9.0 headers common types                              *}
{*                                                                            *}
{*  Direct3DX 9.0 Delphi adaptation by Alexey Barkovoy                        *}
{*  E-Mail: clootie@reactor.ru                                                *}
{*                                                                            *}
{*  Modified: 26-Jan-2003                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*     http://clootie.narod.ru/delphi                                         *}
{*                                                                            *}
{******************************************************************************)
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is DXTypes.pas.                                            }
{                                                                              }
{******************************************************************************}
unit DXTypes;

interface

uses Windows;

type
  // TD3DValue is the fundamental Direct3D fractional data type
  D3DVALUE = Single;
  TD3DValue = D3DVALUE;
  PD3DValue = ^TD3DValue;
  {$NODEFINE D3DVALUE}
  {$NODEFINE TD3DValue}
  {$NODEFINE PD3DValue}

  D3DCOLOR = DWord;
  TD3DColor = D3DCOLOR;
  PD3DColor = ^TD3DColor;
  {$NODEFINE D3DCOLOR}
  {$NODEFINE TD3DColor}
  {$NODEFINE PD3DColor}

  _D3DVECTOR = packed record
    x: Single;
    y: Single;
    z: Single;
  end {_D3DVECTOR};
  D3DVECTOR = _D3DVECTOR;
  TD3DVector = _D3DVECTOR;
  PD3DVector = ^TD3DVector;
  {$NODEFINE _D3DVECTOR}
  {$NODEFINE D3DVECTOR}
  {$NODEFINE TD3DVector}
  {$NODEFINE PD3DVector}

implementation

end.

