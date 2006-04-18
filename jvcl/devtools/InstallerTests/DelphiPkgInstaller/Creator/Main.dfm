object FormMain: TFormMain
  Left = 203
  Top = 121
  BorderStyle = bsDialog
  Caption = 'Binary Component Installer - Creator'
  ClientHeight = 584
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblComponentCount: TLabel
    Left = 328
    Top = 80
    Width = 96
    Height = 13
    Caption = 'LblComponentCount'
  end
  object LblActionCount: TLabel
    Left = 328
    Top = 104
    Width = 72
    Height = 13
    Caption = 'LblActionCount'
  end
  object LblUnitCount: TLabel
    Left = 328
    Top = 128
    Width = 61
    Height = 13
    Caption = 'LblUnitCount'
  end
  object LblPackage: TLabel
    Left = 328
    Top = 40
    Width = 57
    Height = 13
    Caption = 'LblPackage'
  end
  object TreeViewComponents: TTreeView
    Left = 0
    Top = 0
    Width = 321
    Height = 584
    Align = alLeft
    Images = ImageListComponents
    Indent = 27
    TabOrder = 0
  end
  object BtnAdd: TButton
    Left = 328
    Top = 8
    Width = 75
    Height = 25
    Action = ActionAddPackage
    TabOrder = 1
  end
  object Button2: TButton
    Left = 408
    Top = 8
    Width = 75
    Height = 25
    Action = ActionRemovePackage
    TabOrder = 2
  end
  object ProgressBar: TProgressBar
    Left = 328
    Top = 56
    Width = 345
    Height = 17
    TabOrder = 3
  end
  object BtnExport: TButton
    Left = 600
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Export'
    TabOrder = 4
    OnClick = BtnExportClick
  end
  object ImageListComponents: TImageList
    Height = 24
    Width = 24
    Left = 32
    Top = 8
  end
  object OpenDialogPackages: TOpenDialog
    DefaultExt = '.bpl'
    Filter = 'Packages (*.bpl)|*.bpl'
    InitialDir = 'C:\Borland\Delphi7\Projects\Bpl'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Load Packages'
    Left = 80
    Top = 8
  end
  object ActionList1: TActionList
    Left = 536
    Top = 16
    object ActionAddPackage: TAction
      Caption = '&Add'
      OnExecute = ActionAddPackageExecute
    end
    object ActionRemovePackage: TAction
      Caption = '&Remove'
      OnExecute = ActionRemovePackageExecute
      OnUpdate = ActionRemovePackageUpdate
    end
  end
  object SaveDialogExport: TSaveDialog
    DefaultExt = '.xml'
    Filter = 'BinInstaller Config (*:xml)|*.xml'
    InitialDir = 'Z:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Export'
    Left = 120
    Top = 8
  end
end
