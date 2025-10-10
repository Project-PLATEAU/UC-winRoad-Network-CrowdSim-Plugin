unit SimulationInputUtils;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Math,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    PluginCore,
    F8GLUtils;

const
    EDIT_MODE_MESSAGE_ACTION        = 'Ctrlキーを押したままマウスを動かして変更範囲を選択';
    EDIT_MODE_MESSAGE_FINISH        = 'Escキーで終了';
    EDIT_WQ_MESSAGE_ACTION          = '形成範囲内をクリックして待機列を追加';
    SELECT_MODE_MESSAGE_ACTION      = 'Ctrlキーを押したままマウスを動かして待機列形成範囲を選択';
    SELECT_MODE_MESSAGE_CANCEL      = 'Escキーで中断';
    SELECT_EXIT_NODE_MESSAGE_ACTION = '待機列を追加するノードをクリックして選択';
    SELECT_POP_POINT_MESSAGE_ACTION = '発生地点をクリックして選択';
    SELECT_OUT_POINT_MESSAGE_ACTION = '目的地をクリックして選択';
    CHANGE_POINT_MESSAGE_ACTION     = '変更先の地点をクリックして選択';
    SELECT_TERMINAL_MESSAGE_ACTION  = 'ノードをクリックして選択';
    DEFAULT_POINT_NAME              = '経路';
    DEFAULT_POP_POINT_NAME          = '発生地点';
    DEFAULT_OUT_POINT_NAME          = '目的地';

    SELECT_DISPERSION_POINT_MESSAGE_ACTION        = '基準地点をクリックして選択';
    SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION = '任意の発生/退出地点をクリックして選択（複数選択可）';
    CHANGE_DISPERSION_POINT_MESSAGE_ACTION        = '変更先の基準地点をクリックして選択';

    SAMPLE_PEDESTRIAN_CATEGORY_1 = 'Non handicapped adult';
    SAMPLE_PEDESTRIAN_CATEGORY_2 = 'Child or Old person';
    SAMPLE_PEDESTRIAN_CATEGORY_3 = 'Crutch';
    SAMPLE_PEDESTRIAN_CATEGORY_4 = 'Wheel chair';

    NAN_POINT : GLPointType = (NaN, NaN, NaN, NaN);

    MAXIMUM_SIGNAL_INTERVAL = 9999999;
    MOVING_LOG_INTERVAL     = 0.1;
    DEFAULT_ADJUST_RATE     = 1.0;

type
    TGetUpdatedtime  = function : TDateTime of Object;
    TRenderDataArray = array [0..1] of GLPointType;
    TPathNumArray    = array [0..1] of integer;
    TNodeNumArray    = array [0..1] of integer;
    TPopDetailArray  = array [0..23] of integer;

    PopOutPointType = (
        _None,
        _PopPoint,
        _TermOrGate,
        _OutPoint
        );

    PopRuleType = (
        _PopbyInterval,
        _PopbySchedule,
        _PopPerHour
        );

    OutRuleType = (
        _NoRule,
        _OutbyInterval,
        _OutbySchedule
        );

    PointAttrType = (
        _Normal,
        _BusTerminal,
        _TicketGate
        );

implementation

end.
