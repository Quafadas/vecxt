package vecxt.plot

object Fake:
  val fakeData: ujson.Arr = ujson
    .read("""[
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.java_dgemm",
      "score": 8619.293601669751,
      "params": null,
      "scoreLowerConfidence": 8399.14500549159,
      "scoreUpperConfidence": 8839.442197847913,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240917",
      "commit": "6efb42c",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.vecxt_mmult",
      "score": 8804.164838372197,
      "params": null,
      "scoreLowerConfidence": 8422.199092781137,
      "scoreUpperConfidence": 9186.130583963257,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240917",
      "commit": "6efb42c",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 226523684.3941889,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 223161669.04000887,
      "scoreUpperConfidence": 229885699.74836895,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 12447475.617211172,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 11722995.558571529,
      "scoreUpperConfidence": 13171955.675850816,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 85833.6888860225,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": -10932.531755501754,
      "scoreUpperConfidence": 182599.90952754676,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 218844542.36424914,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 207300074.02509958,
      "scoreUpperConfidence": 230389010.7033987,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 9711455.898566278,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 9237113.920849398,
      "scoreUpperConfidence": 10185797.876283158,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 68617.79241906753,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": 66034.77547016251,
      "scoreUpperConfidence": 71200.80936797254,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AndBooleanBenchmark.and_loop",
      "score": 143419197.8459286,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -174792636.52063274,
      "scoreUpperConfidence": 461631032.21248996,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AndBooleanBenchmark.and_loop",
      "score": 13104422.399401993,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": -9726773.871743444,
      "scoreUpperConfidence": 35935618.670547426,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AndBooleanBenchmark.and_loop",
      "score": 1826.4828370975704,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 1796.3653976481314,
      "scoreUpperConfidence": 1856.6002765470093,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AndBooleanBenchmark.and_loop_vec",
      "score": 149913436.31070885,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 24777666.228274345,
      "scoreUpperConfidence": 275049206.39314336,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AndBooleanBenchmark.and_loop_vec",
      "score": 103876663.62626712,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": -1347562.2040197551,
      "scoreUpperConfidence": 209100889.456554,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AndBooleanBenchmark.and_loop_vec",
      "score": 97970.08497937524,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": -215701.2950057836,
      "scoreUpperConfidence": 411641.4649645341,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.CountTrueBenchmark.countTrue_loop",
      "score": 314145780.3178962,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 285660574.9515209,
      "scoreUpperConfidence": 342630985.68427145,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.CountTrueBenchmark.countTrue_loop",
      "score": 13806701.403233439,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": 13259383.346187025,
      "scoreUpperConfidence": 14354019.460279852,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.CountTrueBenchmark.countTrue_loop",
      "score": 16036.967482152924,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 15785.797445814085,
      "scoreUpperConfidence": 16288.137518491763,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.CountTrueBenchmark.countTrue_loop_vec",
      "score": 281647710.4613305,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -262098928.82162064,
      "scoreUpperConfidence": 825394349.7442815,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.CountTrueBenchmark.countTrue_loop_vec",
      "score": 118842053.69757612,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": 58933271.58927811,
      "scoreUpperConfidence": 178750835.80587414,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.CountTrueBenchmark.countTrue_loop_vec",
      "score": 193885.36193539467,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 190942.7726078657,
      "scoreUpperConfidence": 196827.95126292363,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.java_dgemm",
      "score": 8441.064788045542,
      "params": null,
      "scoreLowerConfidence": 5129.900678790769,
      "scoreUpperConfidence": 11752.228897300314,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.vecxt_mmult",
      "score": 8706.690281309006,
      "params": null,
      "scoreLowerConfidence": 8252.741582890676,
      "scoreUpperConfidence": 9160.638979727335,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 203690373.63924637,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -501552691.69193673,
      "scoreUpperConfidence": 908933438.9704294,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 15891513.569928482,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 14133045.855936334,
      "scoreUpperConfidence": 17649981.28392063,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 11692.180376529379,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 10502.365500222166,
      "scoreUpperConfidence": 12881.995252836592,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 164507828.1540434,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -615835045.4882787,
      "scoreUpperConfidence": 944850701.7963656,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 17392363.7198377,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 14556481.337773105,
      "scoreUpperConfidence": 20228246.10190229,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 12967.997409265567,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 11690.618780406743,
      "scoreUpperConfidence": 14245.37603812439,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.LogicalBenchmark.lte_loop",
      "score": 187169649.07602406,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -353220809.81959116,
      "scoreUpperConfidence": 727560107.9716393,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.LogicalBenchmark.lte_loop",
      "score": 19036079.310444072,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": 14054533.895536058,
      "scoreUpperConfidence": 24017624.725352086,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.LogicalBenchmark.lte_loop",
      "score": 24169.565676887203,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 23751.64809867664,
      "scoreUpperConfidence": 24587.483255097766,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.LogicalBenchmark.lte_vec",
      "score": 166250739.02501315,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -335032018.46050465,
      "scoreUpperConfidence": 667533496.510531,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.LogicalBenchmark.lte_vec",
      "score": 5672010.940202254,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": 4678201.980278523,
      "scoreUpperConfidence": 6665819.900125985,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.LogicalBenchmark.lte_vec",
      "score": 7752.824139098273,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 6233.544223882514,
      "scoreUpperConfidence": 9272.104054314032,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.OrBooleanBenchmark.or_loop",
      "score": 159237767.40435418,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -197680725.5157924,
      "scoreUpperConfidence": 516156260.3245008,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.OrBooleanBenchmark.or_loop",
      "score": 9308337.717911892,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": 1201104.5286621312,
      "scoreUpperConfidence": 17415570.907161653,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.OrBooleanBenchmark.or_loop",
      "score": 1861.458936547411,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 1814.5720499553454,
      "scoreUpperConfidence": 1908.3458231394766,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.OrBooleanBenchmark.or_vec",
      "score": 145549682.75937065,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -361244442.2457769,
      "scoreUpperConfidence": 652343807.7645183,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.OrBooleanBenchmark.or_vec",
      "score": 103928416.11804445,
      "params": {
        "len": "128"
      },
      "scoreLowerConfidence": 10164080.650232553,
      "scoreUpperConfidence": 197692751.58585635,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.OrBooleanBenchmark.or_vec",
      "score": 96849.13727138599,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": -191273.1400374449,
      "scoreUpperConfidence": 384971.41458021686,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 452324697.4349596,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 435522831.3854201,
      "scoreUpperConfidence": 469126563.4844991,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 22691314.725780535,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 21854148.426210314,
      "scoreUpperConfidence": 23528481.025350757,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 10666.18048659749,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 10490.949133897328,
      "scoreUpperConfidence": 10841.411839297652,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 462325661.3905508,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 151945346.59304714,
      "scoreUpperConfidence": 772705976.1880544,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 40069655.6656858,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 37129819.985884435,
      "scoreUpperConfidence": 43009491.34548717,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 40014.742422167124,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 39469.64178809725,
      "scoreUpperConfidence": 40559.843056237,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 483164201.70377535,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": -76200831.4075535,
      "scoreUpperConfidence": 1042529234.8151042,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 103597986.25372517,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 43128129.0718777,
      "scoreUpperConfidence": 164067843.43557262,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 42632.53448475978,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 42493.49172412734,
      "scoreUpperConfidence": 42771.57724539222,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20241004",
      "commit": "9af60ba",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 138388003.8953686,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 129012901.97553267,
      "scoreUpperConfidence": 147763105.81520453,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 11951013.609552557,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 10426480.145582752,
      "scoreUpperConfidence": 13475547.073522361,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 86528.40952324326,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": -16250.177538185904,
      "scoreUpperConfidence": 189306.99658467242,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 225547582.14842042,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 215117152.75041923,
      "scoreUpperConfidence": 235978011.54642162,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 11346403.371806012,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 9359940.045133062,
      "scoreUpperConfidence": 13332866.698478963,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 69313.47894141004,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": 63774.94558068164,
      "scoreUpperConfidence": 74852.01230213844,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.java_dgemm",
      "score": 8348.085943308999,
      "params": null,
      "scoreLowerConfidence": 5720.613194227123,
      "scoreUpperConfidence": 10975.558692390874,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.vecxt_mmult",
      "score": 8680.65517196811,
      "params": null,
      "scoreLowerConfidence": 8153.384024552092,
      "scoreUpperConfidence": 9207.926319384127,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 223954018.13702917,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 221846046.35633966,
      "scoreUpperConfidence": 226061989.91771868,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 15114949.56905401,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 14052191.257333139,
      "scoreUpperConfidence": 16177707.88077488,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 11667.135385459536,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 11192.96644219131,
      "scoreUpperConfidence": 12141.304328727763,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 188180365.3867774,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 181907564.65916267,
      "scoreUpperConfidence": 194453166.11439213,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 19339585.79202112,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 14652777.48564089,
      "scoreUpperConfidence": 24026394.098401353,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 13153.581259859617,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 11843.904145848453,
      "scoreUpperConfidence": 14463.25837387078,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 451117596.67177653,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 424809715.82348484,
      "scoreUpperConfidence": 477425477.5200682,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 22731808.03572442,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 21744950.896352325,
      "scoreUpperConfidence": 23718665.175096516,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 10673.447655530179,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 10598.849402904616,
      "scoreUpperConfidence": 10748.045908155742,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 452471365.3796916,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 448038388.2437423,
      "scoreUpperConfidence": 456904342.5156409,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 40016780.33983904,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 38399796.126097076,
      "scoreUpperConfidence": 41633764.55358101,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 39505.99593499847,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 38568.54119493152,
      "scoreUpperConfidence": 40443.450675065425,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 500671903.4009518,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 487801649.0355126,
      "scoreUpperConfidence": 513542157.766391,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 105812459.15042877,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 102846754.90317407,
      "scoreUpperConfidence": 108778163.39768347,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 42621.98019423935,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 42345.85328354004,
      "scoreUpperConfidence": 42898.10710493867,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240923",
      "commit": "b00d1b3",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 139271180.28241995,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 132556875.57415172,
      "scoreUpperConfidence": 145985484.99068817,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 11950437.734585062,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 11427574.826129854,
      "scoreUpperConfidence": 12473300.64304027,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 93389.34406704002,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": 89358.88872650923,
      "scoreUpperConfidence": 97419.7994075708,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 226672928.04083073,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 222034813.98882085,
      "scoreUpperConfidence": 231311042.0928406,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 10687494.824314008,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 10580066.929250302,
      "scoreUpperConfidence": 10794922.719377713,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 69962.0635501234,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": 68626.63993475148,
      "scoreUpperConfidence": 71297.48716549533,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.java_dgemm",
      "score": 8695.833161522669,
      "params": null,
      "scoreLowerConfidence": 5669.514093990583,
      "scoreUpperConfidence": 11722.152229054755,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.vecxt_mmult",
      "score": 8915.458927368965,
      "params": null,
      "scoreLowerConfidence": 8541.425415824586,
      "scoreUpperConfidence": 9289.492438913345,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 454987372.51822996,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 450012818.3419417,
      "scoreUpperConfidence": 459961926.6945182,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 22881036.167108547,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 22451776.43882368,
      "scoreUpperConfidence": 23310295.895393413,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 10733.80671909106,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 10668.792736435029,
      "scoreUpperConfidence": 10798.820701747092,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 454811646.6707771,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 442645296.7622984,
      "scoreUpperConfidence": 466977996.57925576,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 40353921.10599149,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 39806269.92838973,
      "scoreUpperConfidence": 40901572.28359325,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 40141.62098472344,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 39426.93548183463,
      "scoreUpperConfidence": 40856.30648761225,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 505591602.6431393,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 468908681.9808423,
      "scoreUpperConfidence": 542274523.3054363,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 110847968.17268698,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 109484784.64933716,
      "scoreUpperConfidence": 112211151.6960368,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 42821.900761641686,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 42577.607618969814,
      "scoreUpperConfidence": 43066.19390431356,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240918",
      "commit": "ee74ea1",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 139359966.4171197,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 134584046.21217236,
      "scoreUpperConfidence": 144135886.62206706,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 11931583.07094276,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 10686461.508945653,
      "scoreUpperConfidence": 13176704.632939866,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add",
      "score": 84246.77947643935,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": 22129.770280510762,
      "scoreUpperConfidence": 146363.78867236793,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 219839958.7488682,
      "params": {
        "n": "10"
      },
      "scoreLowerConfidence": 211938444.65980825,
      "scoreUpperConfidence": 227741472.83792815,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 9892912.635849932,
      "params": {
        "n": "1000"
      },
      "scoreLowerConfidence": 9410734.69182578,
      "scoreUpperConfidence": 10375090.579874085,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.AddScalarBenchmark.vecxt_add_vec",
      "score": 69348.94123780193,
      "params": {
        "n": "100000"
      },
      "scoreLowerConfidence": 65564.12509877449,
      "scoreUpperConfidence": 73133.75737682938,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.java_dgemm",
      "score": 8537.093891293216,
      "params": null,
      "scoreLowerConfidence": 8134.54270164867,
      "scoreUpperConfidence": 8939.645080937762,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.DgemmBenchmark.vecxt_mmult",
      "score": 8797.664732172592,
      "params": null,
      "scoreLowerConfidence": 8670.786799596566,
      "scoreUpperConfidence": 8924.542664748618,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 224848587.59496346,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 210729770.58527774,
      "scoreUpperConfidence": 238967404.6046492,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 14837768.924758226,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 11635989.019166492,
      "scoreUpperConfidence": 18039548.83034996,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_normal",
      "score": 11633.998061307298,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 10999.630910682992,
      "scoreUpperConfidence": 12268.365211931605,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 188047359.70791328,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 181862343.98820785,
      "scoreUpperConfidence": 194232375.4276187,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 16116134.55952835,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 14791056.451390456,
      "scoreUpperConfidence": 17441212.667666245,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.IncrementBenchmark.increment_vec",
      "score": 12717.920536802108,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 11672.098443947918,
      "scoreUpperConfidence": 13763.742629656299,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 452486620.06754166,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 423967046.8460835,
      "scoreUpperConfidence": 481006193.2889998,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 22706390.159225523,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 22348444.738475405,
      "scoreUpperConfidence": 23064335.579975642,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_loop",
      "score": 10676.541582433303,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 10568.445652728733,
      "scoreUpperConfidence": 10784.637512137873,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 453218721.82457656,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 435018518.79841256,
      "scoreUpperConfidence": 471418924.85074055,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 39986464.34915977,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 39095356.4480061,
      "scoreUpperConfidence": 40877572.25031344,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec",
      "score": 39862.73204940008,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 39319.171143748164,
      "scoreUpperConfidence": 40406.292955052,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 501708801.29142565,
      "params": {
        "len": "3"
      },
      "scoreLowerConfidence": 457254277.9398022,
      "scoreUpperConfidence": 546163324.643049,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 105263506.24056552,
      "params": {
        "len": "100"
      },
      "scoreLowerConfidence": 101476521.90125777,
      "scoreUpperConfidence": 109050490.57987328,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    },
    {
      "benchmark": "vecxt.benchmark.SumBenchmark.sum_vec_alt",
      "score": 42568.481369110574,
      "params": {
        "len": "100000"
      },
      "scoreLowerConfidence": 42475.64805895111,
      "scoreUpperConfidence": 42661.314679270035,
      "scoreUnit": "ops/s",
      "branch": "main",
      "date": "20240924",
      "commit": "ff3686f",
      "host": "gha"
    }
  ]""".trim())
    .arr
end Fake
