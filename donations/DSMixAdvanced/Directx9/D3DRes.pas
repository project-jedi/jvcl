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
// File: D3DRes.h
//
// Desc: Resource definitions required by the CD3DApplication class.
//       Any application using the CD3DApplication class must include resources
//       with the following identifiers.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
unit D3DRes;

interface

const
  IDI_MAIN_ICON          = 101; // Application icon
  IDR_MAIN_ACCEL         = 113; // Keyboard accelerator
  IDR_MENU               = 141; // Application menu
  IDR_POPUP              = 142; // Popup menu
  IDD_SELECTDEVICE       = 144; // "Change Device" dialog box

  IDC_ADAPTER_COMBO         = 1002; // Adapter combobox for "SelectDevice" dlg
  IDC_DEVICE_COMBO          = 1000; // Device combobox for "SelectDevice" dlg
  IDC_ADAPTERFORMAT_COMBO   = 1003;
  IDC_RESOLUTION_COMBO      = 1004;
  IDC_MULTISAMPLE_COMBO     = 1005; // MultiSample combobox for "SelectDevice" dlg
  IDC_REFRESHRATE_COMBO     = 1006;
  IDC_BACKBUFFERFORMAT_COMBO = 1007;
  IDC_DEPTHSTENCILBUFFERFORMAT_COMBO = 1008;
  IDC_VERTEXPROCESSING_COMBO = 1009;
  IDC_PRESENTINTERVAL_COMBO = 1010;
  IDC_MULTISAMPLE_QUALITY_COMBO   = 1011;
  IDC_WINDOW                = 1016; // Radio button for windowed-mode
  IDC_FULLSCREEN            = 1018; // Radio button for fullscreen-mode

  IDM_CHANGEDEVICE     = 40002; // Command to invoke "Change Device" dlg
  IDM_TOGGLEFULLSCREEN = 40003; // Command to toggle fullscreen mode
  IDM_TOGGLESTART      = 40004; // Command to toggle frame animation
  IDM_SINGLESTEP       = 40005; // Command to single step frame animation
  IDM_EXIT             = 40006; // Command to exit the application

implementation

end.
