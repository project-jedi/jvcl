object ADS70_Data: TADS70_Data
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 761
  Top = 170
  Height = 245
  Width = 214
  object adsD: TAdsDictionary
    AdsServerTypes = [stADS_LOCAL]
    IsConnected = False
    ConnectPath = 
      'C:\RaysDelphiJediProjects\EDI\EDISource\EDIDBBuffering\EDIDataba' +
      'se\EDI_SDKDB.add'
    LoginPrompt = False
    UserName = 'adssys'
    Left = 24
    Top = 12
  end
  object adsC: TAdsConnection
    ConnectPath = 
      'C:\RaysDelphiJediProjects\EDI\EDISource\EDIDBBuffering\EDIDataba' +
      'se\EDI_SDKDB.add'
    AdsServerTypes = [stADS_LOCAL]
    LoginPrompt = False
    Username = 'adssys'
    StoreConnected = False
    Compression = ccAdsCompressionNotSet
    Left = 52
    Top = 12
  end
  object LProfile: TAdsQuery
    DatabaseName = 'adsC'
    StoreActive = False
    AdsTableOptions.AdsFilterOptions = RESPECT_WHEN_COUNTING
    AdsTableOptions.AdsFreshRecordCount = True
    AdsTableOptions.AdsScopeOptions = RESPECT_SCOPES_WHEN_COUNTING
    RequestLive = True
    SQL.Strings = (
      'select * from ProfileLoop')
    Left = 124
    Top = 56
    ParamData = <>
  end
  object SProfile: TAdsQuery
    DatabaseName = 'adsC'
    StoreActive = False
    AdsTableOptions.AdsFilterOptions = RESPECT_WHEN_COUNTING
    AdsTableOptions.AdsFreshRecordCount = True
    AdsTableOptions.AdsScopeOptions = RESPECT_SCOPES_WHEN_COUNTING
    RequestLive = True
    SQL.Strings = (
      'select * from ProfileSeg')
    Left = 76
    Top = 56
    ParamData = <>
  end
  object EProfile: TAdsQuery
    DatabaseName = 'adsC'
    StoreActive = False
    AdsTableOptions.AdsFilterOptions = RESPECT_WHEN_COUNTING
    AdsTableOptions.AdsFreshRecordCount = True
    AdsTableOptions.AdsScopeOptions = RESPECT_SCOPES_WHEN_COUNTING
    RequestLive = True
    SQL.Strings = (
      'select * from ProfileEle')
    Left = 24
    Top = 56
    ParamData = <>
  end
  object dsLProfile: TDataSource
    DataSet = LProfile
    Left = 124
    Top = 104
  end
  object dsSProfile: TDataSource
    DataSet = SProfile
    Left = 76
    Top = 104
  end
  object dsEProfile: TDataSource
    DataSet = EProfile
    Left = 24
    Top = 108
  end
  object JvEDIDBBuffer: TJvEDIDBBuffer
    ElementProfiles = EProfile
    SegmentProfiles = SProfile
    LoopProfiles = LProfile
    KeySuffix = '_Id'
    LoopKeyPrefix = 'Loop_'
    ElementNonKeyPrefix = 'E'
    OnAfterOpenDataSets = JvEDIDBBufferAfterOpenDataSets
    OnTableExists = JvEDIDBBufferTableExists
    OnCreateTable = JvEDIDBBufferCreateTable
    OnCheckForFieldChanges = JvEDIDBBufferCheckForFieldChanges
    OnAlterTable = JvEDIDBBufferAlterTable
    Left = 32
    Top = 160
  end
end
