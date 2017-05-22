object ClientAssistant: TClientAssistant
  Left = 322
  Top = 178
  BorderStyle = bsDialog
  Caption = 'TJvPageManager  Demo'
  ClientHeight = 335
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefaultPosOnly
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnPanel: TPanel
    Left = 0
    Top = 290
    Width = 493
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 493
      Height = 13
      Align = alTop
      Shape = bsTopLine
    end
    object SaveBtn: TButton
      Left = 324
      Top = 12
      Width = 77
      Height = 25
      Caption = '&Save'
      TabOrder = 2
      Visible = False
      OnClick = SaveBtnClick
    end
    object BackBtn: TButton
      Left = 8
      Top = 12
      Width = 77
      Height = 25
      Caption = '< &Back'
      TabOrder = 0
    end
    object NextBtn: TButton
      Left = 84
      Top = 12
      Width = 77
      Height = 25
      Caption = '&Next >'
      Default = True
      TabOrder = 1
    end
    object CloseBtn: TButton
      Left = 408
      Top = 12
      Width = 77
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 3
      OnClick = CloseBtnClick
    end
  end
  object Notebook: TNotebook
    Left = 0
    Top = 0
    Width = 493
    Height = 290
    Align = alClient
    TabOrder = 1
    object TPage
      Left = 0
      Top = 0
      Caption = 'ToDo'
      object Image1: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000008000000000000
          0000000000000000000800000000000000000000000000000007800000000000
          0000000000000000000780008000000000000000000000000007780080000000
          0000000000000888880778007800080000000000000000000007778078000800
          00000000000088888877778077800780000000000000B7777777777807800780
          000000000000B7777777777807780778000000000000B77777777777B0780778
          000000000000B7777777777B07778077800000000000B7777777777B07778077
          800000000000BBBBBBBB77B077777B077800000000000000000B77B07777B077
          78000000000000000B0B7B077777B07777B00000000000000B0B7B0BB77B0777
          7B00000000000000000BB000B77B07777B00000000000000000BB0B0B7B0BB77
          B000000000000000000B0000B7B00B77B000000000000000000B0000BB000B7B
          000000000000000000000000BB000B7B000000000000000000000000B0000BB0
          000000000000000000000000B0000BB000000000000000000000000000000B00
          00000000000000000000000000000B0000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000FFFFFFFFFFDFFFFFFFCFFFFFFFC7FFFFFFC6FFFFFFC27FFFFFC237FF
          FFC033FFF80011FFE00011FFE00000FFE00000FFE000007FE000007FE000003F
          E000003FE000001FE000001FFF00000FFF00001FFF80001FFFC0003FFFC4003F
          FFC6107FFFCE107FFFDE30FFFFFE30FFFFFE71FFFFFEF1FFFFFFF3FFFFFFF7FF
          FFFFFFFF}
      end
      object Label1: TLabel
        Left = 56
        Top = 12
        Width = 425
        Height = 53
        AutoSize = False
        Caption = 
          'This is the TJvPageManager  component demonstration form. You ca' +
          'n use '#39'Next'#39' or '#39'Back'#39' buttons to move between operation stadies' +
          '.'
        WordWrap = True
      end
      object Label16: TLabel
        Left = 109
        Top = 100
        Width = 321
        Height = 29
        AutoSize = False
        Caption = 'Choose an operation below:  '
        WordWrap = True
      end
      object NewBtn: TRadioButton
        Left = 109
        Top = 144
        Width = 250
        Height = 17
        Caption = 'Input new customer data'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object EditBtn: TRadioButton
        Left = 109
        Top = 164
        Width = 250
        Height = 17
        Caption = 'Change selected customer data'
        TabOrder = 1
      end
      object DeleteBtn: TRadioButton
        Left = 109
        Top = 184
        Width = 250
        Height = 17
        Caption = 'Delete information about selected customer'
        TabOrder = 2
      end
      object ViewBtn: TRadioButton
        Left = 109
        Top = 204
        Width = 250
        Height = 17
        Caption = 'View customer data (read-only)'
        TabOrder = 3
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'SearchConfirm'
      object NewSearchHint: TLabel
        Left = 56
        Top = 12
        Width = 421
        Height = 41
        AutoSize = False
        Caption = 
          'Customer data to input may already be stored in database. You ca' +
          'n check presence of this customer data in database by specifying' +
          ' selection criteria.'
        WordWrap = True
      end
      object Image3: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF00000000000000FFFFFFFFFFFFFFFFFFFF000000000000BFBFBFBFBFBF
          00BFBFBF000000000000FFFFFFFFFFF0E60FFFFF000000000000BFBFBFBFBFBF
          00BFBFBF000000000000FFFFFFFFFFFFFFFFFFFF000000000000BFBFBFBFBFBF
          00BFBFBF000000000000FFFFFFFFFFF0E60FFFFF000000000000BFBFBFBFBFB0
          E60FBFBF000000000000FFFFFFFFFFF0E60FFFFF000000000000BFBFBFBFBFB0
          E60FBFBF0000000000000FFFFFFFFFFF0E60FFFF00000000000118BFBFBF00BF
          B0E60FBF000000000001108FFFF0EE0FF0E60FFF0000000000011108BFB0EEE0
          066E0FBF00000000000111108FFF0EEEEEE0FFFF000000000001111108BFB000
          000FBFBF0000000008011111998FFFFFFFFFFFFF00000000000811119908BFBF
          BFBFBFBF000000008077711999908FFFFFFFFFFF000000080777FFF999999000
          0000000000000080777FFFF999999000000000000000080777FFFF0000000000
          00000000000080777FFFF008000000000000000000080777FFFF080000000000
          000000000080777FFFF090000000000000000000080777FFFF09990000000000
          0000000008077FFFF099000000000000000000000887FFFF0890000000000000
          000000000008FFF0800000000000000000000000000080080000000000000000
          0000000000008880000000000000000000000000000000000000000000000000
          00000000FFF00000FFF00000FFF00000FFF00000FFF00000FFF00000FFF00000
          FFF00000FFF00000FFF00000FFE00000FFC00000FFC00000FFC00000FFC00000
          FFC00000FF800000FF800000FF000000FE0003FFFC0003FFF80007FFF000FFFF
          E003FFFFC007FFFF8003FFFF800FFFFF801FFFFFC07FFFFFE0FFFFFFF1FFFFFF
          FFFFFFFF}
      end
      object Label24: TLabel
        Left = 56
        Top = 86
        Width = 421
        Height = 41
        AutoSize = False
        Caption = 
          'Select operation from the list below and click "Next" to continu' +
          'e.'
        WordWrap = True
      end
      object DoSearchBtn: TRadioButton
        Left = 64
        Top = 126
        Width = 373
        Height = 17
        Caption = 'Search customer in database (strongly recommended)'
        TabOrder = 0
      end
      object SkipSearchBtn: TRadioButton
        Left = 64
        Top = 154
        Width = 373
        Height = 17
        Caption = 'Start to input customer data, don'#39't search.'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'SearchParams'
      object Image2: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000BFBFBFBFBFBF
          BFBFBFB0000000000000FFFFFFFFFFFFFFFFFFF0000000000000BFBFBFBFBFBF
          BFBFBFB0000000000000FFFFFFFFFFFFFFFFFFF0000000000000BFBFBFBFBFBF
          BFBFBFB0000000000000FFFFFFFFFFFFFFFFFFF0000000000000BFBFBFBFBFBF
          BFBFBFB0000000000000FFFFFFFFFFFFFFFFFFF0000000000000BFBFBFBFBFBF
          BFBFBFB00000000000000FFFFFFFFFFFFFFFFFF000000000000118BFBFBFBFBF
          BFBFBFB0000000000001108FFFFFFFFFFFFFFFF00000000000011108BFBFBFBF
          BFBFBFB000000000000111108FFFFFFFFFFFFFF0000000000001111108BFBFBF
          BFBFBFB00000000008011111998FFFFFFFFFFFF000000000000811119908BFBF
          BFBFBFB0000000008077711999908FFFFFFFFFF0000000080777FFF999999000
          0000000000000080777FFFF999999000000000000000080777FFFF0000000000
          00000000000080777FFFF008000000000000000000080777FFFF080000000000
          000000000080777FFFF090000000000000000000080777FFFF09990000000000
          0000000008077FFFF099000000000000000000000887FFFF0890000000000000
          000000000008FFF0800000000000000000000000000080080000000000000000
          0000000000008880000000000000000000000000000000000000000000000000
          00000000FFFFFFFFFFF00001FFF00001FFF00001FFF00001FFF00001FFF00001
          FFF00001FFF00001FFF00001FFE00001FFC00001FFC00001FFC00001FFC00001
          FFC00001FF800001FF800001FF000001FE0003FFFC0003FFF80007FFF000FFFF
          E003FFFFC007FFFF8003FFFF800FFFFF801FFFFFC07FFFFFE0FFFFFFF1FFFFFF
          FFFFFFFF}
      end
      object Label17: TLabel
        Left = 56
        Top = 12
        Width = 425
        Height = 45
        AutoSize = False
        Caption = 
          'Input selection criteria in the fields below. Click "Next" to st' +
          'art search. Click "Back" to change type of operation.'
        WordWrap = True
      end
      object Label19: TLabel
        Left = 56
        Top = 107
        Width = 54
        Height = 13
        Caption = '&Last Name:'
        FocusControl = SrchName
      end
      object Label20: TLabel
        Left = 56
        Top = 131
        Width = 53
        Height = 13
        Caption = '&First Name:'
        FocusControl = SrchFirstName
      end
      object Label21: TLabel
        Left = 56
        Top = 155
        Width = 20
        Height = 13
        Caption = '&City:'
        FocusControl = SrchCity
      end
      object Label23: TLabel
        Left = 56
        Top = 179
        Width = 25
        Height = 13
        Caption = '&State'
        FocusControl = SrchState
      end
      object Label27: TLabel
        Left = 56
        Top = 203
        Width = 55
        Height = 13
        Caption = '&Occupation'
        FocusControl = SrchOccupation
      end
      object SrchName: TEdit
        Left = 144
        Top = 104
        Width = 150
        Height = 19
        TabOrder = 0
      end
      object SrchFirstName: TEdit
        Left = 144
        Top = 128
        Width = 150
        Height = 19
        TabOrder = 1
      end
      object SrchCity: TEdit
        Left = 144
        Top = 152
        Width = 150
        Height = 19
        TabOrder = 2
      end
      object SrchState: TEdit
        Left = 144
        Top = 176
        Width = 150
        Height = 19
        TabOrder = 3
      end
      object SrchOccupation: TEdit
        Left = 144
        Top = 200
        Width = 150
        Height = 19
        TabOrder = 4
      end
      object ExactBtn: TCheckBox
        Left = 144
        Top = 228
        Width = 141
        Height = 19
        Caption = ' &Exact search'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ClientsBrowse'
      object Image4: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF00000000000000FFFFFFFFFFFFFFFFFFFF000000000000BFBFBFBFBFBF
          BFBFBFBF000000000000FFFFFFF000000000FFFF000000000000BFBFBFB0FFFF
          FFF0BFBF000000000000FFFFFFF0F00000F0FFFF000000000000BFBFBFB0FFFF
          FFF0BFBF000000000000FFFFFFF0F00000F0FFFF000000000000BFBFBFB0FFFF
          FFF0BFBF000000000000FFFFFFF0F000FFF0FFFF000000000000BFBFBFB0FFFF
          F000BFBF0000000000000FFFFFF0F00FF0F0FFFF00000000000118BFBFB0FFFF
          F00FBFBF000000000001108FFFF0000000FFFFFF0000000000011108BFBFBFBF
          BFBFBFBF00000000000111108FFFFFFFFFFFFFFF000000000001111108BFBFBF
          BFBFBFBF0000000008011111998FFFFFFFFFFFFF00000000000811119908BFBF
          BFBFBFBF000000008077711999908FFFFFFFFFFF000000080777FFF999999000
          0000000000000080777FFFF999999000000000000000080777FFFF0000000000
          00000000000080777FFFF008000000000000000000080777FFFF080000000000
          000000000080777FFFF090000000000000000000080777FFFF09990000000000
          0000000008077FFFF099000000000000000000000887FFFF0890000000000000
          000000000008FFF0800000000000000000000000000080080000000000000000
          0000000000008880000000000000000000000000000000000000000000000000
          00000000FFF00000FFF00000FFF00000FFF00000FFF00000FFF00000FFF00000
          FFF00000FFF00000FFF00000FFE00000FFC00000FFC00000FFC00000FFC00000
          FFC00000FF800000FF800000FF000000FE0003FFFC0003FFF80007FFF000FFFF
          E003FFFFC007FFFF8003FFFF800FFFFF801FFFFFC07FFFFFE0FFFFFFF1FFFFFF
          FFFFFFFF}
      end
      object Label25: TLabel
        Left = 56
        Top = 12
        Width = 421
        Height = 29
        AutoSize = False
        Caption = 
          'Select a customer from the list below and then click "Next" to c' +
          'ontinue. Click "Back" to return to the previous screen if you wa' +
          'nt to change selection criteria.'
        WordWrap = True
      end
      object Grid: TJvDBGrid
        Left = 4
        Top = 60
        Width = 485
        Height = 221
        DataSource = SearchQueryDS
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        IniStorage = FormStorage
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ClientEdit'
      object Image5: TImage
        Left = 12
        Top = 12
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000000020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000099999990000000000099999
          9999990000999999900000999000999999999090009999999900099999990999
          9999999009099999990999000009999999999990099999999909900999900999
          9999999009999999999000090999009999999990009999999999900090999009
          9999990000999999000090000000000099999900009999909099099000000000
          099990000999999099999990FF0FF0FF099900000999099900000099000FF0FF
          099909000999909077777709990FF00099999900009999077777777090000999
          9999900009099077777707099999999999999000099990777777707099999999
          99000000009999077777777000999999999000000099999077777770F0000999
          99007700000999990000070FF000900000777770077000000F00070FF0009000
          0070707007770000FF00070FF00099000077007007777000FF009090FFF09900
          0007777007770000FFF09090FFF099900BB000000000B0090FF09090FF099990
          BBBBBBB00BBBB0990F0990990F099900BBBBBBB00BBBB0999009990990999990
          BBBBBBB00BBBBB09990999999909990BBBBBBBB00BBBBB009909990999909900
          BBBBBBB00BBBBB09999999099999990BBBBBBBB00BBBBB09999990B0999990BB
          BBBBBBB00BBBBBB099990BB099990BBBBBBBBBB0000000000000000000000000
          0000000000FFE000007C40000038000000000000000000000000000000000000
          8000000100000001000000010000000100000001000000018000000300000003
          0000000780000003800000018000000000000000000000000000000000000001
          8000000000000000000000000000000000000000000000000000000000000000
          000000000000}
      end
      object Label2: TLabel
        Left = 58
        Top = 57
        Width = 78
        Height = 13
        Caption = 'Account number'
        FocusControl = EditACCT_NBR
      end
      object Label3: TLabel
        Left = 108
        Top = 83
        Width = 28
        Height = 13
        Caption = 'Name'
        FocusControl = EditFIRST_NAME
      end
      object Label4: TLabel
        Left = 98
        Top = 107
        Width = 38
        Height = 13
        Caption = 'Address'
        FocusControl = EditADDRESS_
      end
      object Label8: TLabel
        Left = 85
        Top = 157
        Width = 51
        Height = 13
        Caption = 'Telephone'
        FocusControl = EditTELEPHONE
      end
      object Label11: TLabel
        Left = 182
        Top = 139
        Width = 47
        Height = 13
        Caption = 'PICTURE'
      end
      object Label12: TLabel
        Left = 91
        Top = 179
        Width = 45
        Height = 13
        Caption = 'Birth date'
        FocusControl = EditBIRTH_DATE
      end
      object Label14: TLabel
        Left = 81
        Top = 204
        Width = 55
        Height = 13
        Caption = 'Occupation'
        FocusControl = EditOCCUPATION
      end
      object Label9: TLabel
        Left = 203
        Top = 58
        Width = 62
        Height = 13
        Caption = 'Date opened'
        FocusControl = EditDATE_OPEN
      end
      object Label10: TLabel
        Left = 245
        Top = 157
        Width = 24
        Height = 13
        Caption = 'SS #'
        FocusControl = EditSS_NUMBER
      end
      object Label13: TLabel
        Left = 90
        Top = 229
        Width = 46
        Height = 13
        Caption = 'Risk level'
        FocusControl = ComboRISK_LEVEL
      end
      object Label15: TLabel
        Left = 238
        Top = 229
        Width = 50
        Height = 13
        Caption = 'Objectives'
        FocusControl = EditOBJECTIVES
      end
      object Label5: TLabel
        Left = 96
        Top = 253
        Width = 40
        Height = 13
        Caption = 'Interests'
        FocusControl = EditINTERESTS
      end
      object RxLabel1: TJvLabel
        Left = 142
        Top = 17
        Width = 293
        Height = 24
        AutoSize = False
        Caption = 'Customer Details'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ShadowPos = spRightBottom
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -16
        HotTrackFont.Name = 'Arial'
        HotTrackFont.Style = []
        ImageIndex = 0
      end
      object EditACCT_NBR: TDBEdit
        Left = 142
        Top = 55
        Width = 55
        Height = 21
        DataField = 'ACCT_NBR'
        DataSource = ClientsDS
        TabOrder = 0
        OnChange = EditChange
      end
      object EditFIRST_NAME: TDBEdit
        Left = 142
        Top = 80
        Width = 100
        Height = 21
        DataField = 'FIRST_NAME'
        DataSource = ClientsDS
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnChange = EditChange
      end
      object EditADDRESS_: TDBEdit
        Left = 142
        Top = 105
        Width = 222
        Height = 21
        DataField = 'ADDRESS_1'
        DataSource = ClientsDS
        TabOrder = 4
        OnChange = EditChange
      end
      object EditCITY: TDBEdit
        Left = 142
        Top = 129
        Width = 100
        Height = 21
        DataField = 'CITY'
        DataSource = ClientsDS
        TabOrder = 5
        OnChange = EditChange
      end
      object EditSTATE: TDBEdit
        Left = 249
        Top = 129
        Width = 24
        Height = 21
        DataField = 'STATE'
        DataSource = ClientsDS
        TabOrder = 6
        OnChange = EditChange
      end
      object EditZIP: TDBEdit
        Left = 280
        Top = 129
        Width = 84
        Height = 21
        DataField = 'ZIP'
        DataSource = ClientsDS
        TabOrder = 7
        OnChange = EditChange
      end
      object EditTELEPHONE: TDBEdit
        Left = 142
        Top = 153
        Width = 80
        Height = 21
        DataField = 'TELEPHONE'
        DataSource = ClientsDS
        TabOrder = 8
        OnChange = EditChange
      end
      object EditOCCUPATION: TDBEdit
        Left = 142
        Top = 202
        Width = 110
        Height = 21
        DataField = 'OCCUPATION'
        DataSource = ClientsDS
        TabOrder = 11
        OnChange = EditChange
      end
      object EditLAST_NAME: TDBEdit
        Left = 247
        Top = 80
        Width = 117
        Height = 21
        DataField = 'LAST_NAME'
        DataSource = ClientsDS
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnChange = EditChange
      end
      object EditSS_NUMBER: TDBEdit
        Left = 277
        Top = 153
        Width = 87
        Height = 21
        DataField = 'SS_NUMBER'
        DataSource = ClientsDS
        TabOrder = 9
        OnChange = EditChange
      end
      object EditOBJECTIVES: TDBEdit
        Left = 296
        Top = 226
        Width = 67
        Height = 21
        DataField = 'OBJECTIVES'
        DataSource = ClientsDS
        TabOrder = 13
        OnChange = EditChange
      end
      object EditBIRTH_DATE: TJvDBDateEdit
        Left = 142
        Top = 177
        Width = 110
        Height = 21
        DataField = 'BIRTH_DATE'
        DataSource = ClientsDS
        ButtonHint = 'Calendar|'
        DialogTitle = 'Select a date'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        OnChange = EditChange
      end
      object EditINTERESTS: TDBEdit
        Left = 142
        Top = 250
        Width = 221
        Height = 21
        DataField = 'INTERESTS'
        DataSource = ClientsDS
        TabOrder = 14
        OnChange = EditChange
      end
      object EditIMAGE: TDBImage
        Left = 373
        Top = 55
        Width = 105
        Height = 105
        DataField = 'IMAGE'
        DataSource = ClientsDS
        TabOrder = 15
      end
      object EditDATE_OPEN: TJvDBDateEdit
        Left = 271
        Top = 55
        Width = 93
        Height = 21
        DataField = 'DATE_OPEN'
        DataSource = ClientsDS
        ButtonHint = 'Calendar|'
        DialogTitle = 'Select a date'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = EditChange
      end
      object ComboRISK_LEVEL: TJvDBComboBox
        Left = 142
        Top = 226
        Width = 87
        Height = 21
        Hint = 'TJvDBComboBox|'
        DataField = 'RISK_LEVEL'
        DataSource = ClientsDS
        ItemHeight = 13
        Items.Strings = (
          'Low'
          'Medium'
          'High')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        Values.Strings = (
          'LOW'
          'MED'
          'HIGH')
        OnChange = EditChange
      end
    end
  end
  object ClientsDS: TDataSource
    AutoEdit = False
    DataSet = Clients
    Left = 176
    Top = 304
  end
  object SearchQuery: TJvQuery
    DatabaseName = 'DBDEMOS'
    SQL.Strings = (
      'SELECT * FROM '
      '  "CLIENTS.DBF" CLIENTS'
      'WHERE '
      '  (ACCT_NBR IS NOT NULL)'
      '   AND'
      '  (%FIRST_NAME)'
      '   AND'
      '  (%LAST_NAME)'
      '   AND'
      '  (%CITY)'
      '   AND'
      '  (%STATE)'
      '   AND'
      '  (%OCCUPATION)'
      'ORDER BY '
      '  CLIENTS."LAST_NAME",'
      '  CLIENTS."FIRST_NAME"')
    Macros = <
      item
        DataType = ftString
        Name = 'FIRST_NAME'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'LAST_NAME'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'CITY'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'STATE'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'OCCUPATION'
        ParamType = ptInput
        Value = '0=0'
      end>
    Left = 208
    Top = 292
    object SearchQueryLAST_NAME: TStringField
      DisplayLabel = 'Last Name'
      FieldName = 'LAST_NAME'
    end
    object SearchQueryFIRST_NAME: TStringField
      DisplayLabel = 'First Name'
      FieldName = 'FIRST_NAME'
    end
    object SearchQueryACCT_NBR: TFloatField
      DisplayLabel = 'Account'
      FieldName = 'ACCT_NBR'
    end
    object SearchQueryADDRESS_1: TStringField
      DisplayLabel = 'Address'
      FieldName = 'ADDRESS_1'
    end
    object SearchQueryCITY: TStringField
      DisplayLabel = 'City'
      FieldName = 'CITY'
    end
    object SearchQuerySTATE: TStringField
      DisplayLabel = 'State'
      FieldName = 'STATE'
      Size = 2
    end
    object SearchQueryZIP: TStringField
      FieldName = 'ZIP'
      Size = 5
    end
    object SearchQueryTELEPHONE: TStringField
      DisplayLabel = 'Phone'
      FieldName = 'TELEPHONE'
      Size = 12
    end
    object SearchQueryDATE_OPEN: TDateField
      DisplayLabel = 'Date opened'
      FieldName = 'DATE_OPEN'
    end
    object SearchQuerySS_NUMBER: TFloatField
      DisplayLabel = 'SS #'
      FieldName = 'SS_NUMBER'
    end
    object SearchQueryBIRTH_DATE: TDateField
      DisplayLabel = 'Birth Date'
      FieldName = 'BIRTH_DATE'
    end
    object SearchQueryRISK_LEVEL: TStringField
      DisplayLabel = 'Risk Level'
      FieldName = 'RISK_LEVEL'
      Size = 8
    end
    object SearchQueryOCCUPATION: TStringField
      DisplayLabel = 'Occupation'
      FieldName = 'OCCUPATION'
    end
    object SearchQueryOBJECTIVES: TStringField
      DisplayLabel = 'Objectives'
      FieldName = 'OBJECTIVES'
      Size = 10
    end
    object SearchQueryINTERESTS: TStringField
      DisplayLabel = 'Interests'
      FieldName = 'INTERESTS'
      Size = 120
    end
  end
  object SearchQueryDS: TDataSource
    DataSet = SearchQuery
    Left = 248
    Top = 296
  end
  object FormStorage: TJvFormStorage
    AppStorage = MainForm.JvAppRegistryStorage
    AppStoragePath = 'Page Manager\'
    StoredProps.Strings = (
      'DoSearchBtn.Checked'
      'SkipSearchBtn.Checked'
      'ExactBtn.Checked')
    StoredValues = <>
    Left = 4
    Top = 294
  end
  object PageManager: TJvPageManager
    PageOwner = Notebook
    NextBtn = NextBtn
    PriorBtn = BackBtn
    OnGetPriorPage = PageManagerGetPriorPage
    OnGetNextPage = PageManagerGetNextPage
    Left = 32
    Top = 294
    object ToDo: TJvPageProxy
      PageName = 'ToDo'
      OnLeave = ToDoLeave
    end
    object SearchConfirm: TJvPageProxy
      PageName = 'SearchConfirm'
    end
    object SearchParams: TJvPageProxy
      PageName = 'SearchParams'
      OnLeave = SearchParamsLeave
    end
    object ClientsBrowse: TJvPageProxy
      PageName = 'ClientsBrowse'
      OnLeave = ClientsBrowseLeave
      OnShow = ClientsBrowseShow
    end
    object ClientEdit: TJvPageProxy
      PageName = 'ClientEdit'
      OnEnter = ClientEditEnter
      OnLeave = ClientEditLeave
      OnShow = ClientEditShow
    end
  end
  object Clients: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'CLIENTS.DBF'
    Left = 176
    Top = 294
    object ClientsLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Required = True
      OnChange = ClientDataChange
    end
    object ClientsFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Required = True
      OnChange = ClientDataChange
    end
    object ClientsACCT_NBR: TFloatField
      FieldName = 'ACCT_NBR'
      Required = True
      OnChange = ClientDataChange
    end
    object ClientsADDRESS_1: TStringField
      FieldName = 'ADDRESS_1'
      OnChange = ClientDataChange
    end
    object ClientsCITY: TStringField
      FieldName = 'CITY'
      OnChange = ClientDataChange
    end
    object ClientsSTATE: TStringField
      FieldName = 'STATE'
      OnChange = ClientDataChange
      Size = 2
    end
    object ClientsZIP: TStringField
      FieldName = 'ZIP'
      OnChange = ClientDataChange
      Size = 5
    end
    object ClientsTELEPHONE: TStringField
      FieldName = 'TELEPHONE'
      OnChange = ClientDataChange
      Size = 12
    end
    object ClientsDATE_OPEN: TDateField
      FieldName = 'DATE_OPEN'
      OnChange = ClientDataChange
    end
    object ClientsSS_NUMBER: TFloatField
      FieldName = 'SS_NUMBER'
      OnChange = ClientDataChange
    end
    object ClientsPICTURE: TStringField
      FieldName = 'PICTURE'
      OnChange = ClientDataChange
      Size = 15
    end
    object ClientsBIRTH_DATE: TDateField
      FieldName = 'BIRTH_DATE'
      OnChange = ClientDataChange
    end
    object ClientsRISK_LEVEL: TStringField
      FieldName = 'RISK_LEVEL'
      OnChange = ClientDataChange
      Size = 8
    end
    object ClientsOCCUPATION: TStringField
      FieldName = 'OCCUPATION'
      OnChange = ClientDataChange
    end
    object ClientsOBJECTIVES: TStringField
      FieldName = 'OBJECTIVES'
      OnChange = ClientDataChange
      Size = 10
    end
    object ClientsINTERESTS: TStringField
      FieldName = 'INTERESTS'
      OnChange = ClientDataChange
      Size = 120
    end
    object ClientsIMAGE: TBlobField
      FieldName = 'IMAGE'
      OnChange = ClientDataChange
      Size = 1
    end
  end
end
