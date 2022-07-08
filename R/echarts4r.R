#' @title Readble for Humans
#'
#' Convert machine numbers (too big or too small) to human readable numbers.
#'
#' @param type can be tooltip or axis, formatter.
#'
#' @export
e_format_num <- function(type = "tooltip") {

    if (type == "axis") {
        stringr::str_c(
            "
            function(value, index) {
                var SI_SYMBOL = ['', 'k', 'M', 'B', 'T', 'Q'];
                function abbreviateNumber(number){

                    // what tier? (determines SI symbol)
                    var tier = Math.log10(Math.abs(number)) / 3 | 0;

                    // if zero, we don't need a suffix
                    if(tier == 0) return number;

                    // get suffix and determine scale
                    var suffix = SI_SYMBOL[tier];
                    var scale = Math.pow(10, tier * 3);

                    // scale the number
                    var scaled = number / scale;

                    // format number and add suffix
                    return scaled.toFixed(0) + suffix;
                };
                return abbreviateNumber(value);
            }"
        )
    } else if (type == "tooltip") {
        stringr::str_c(
            "function(params){
                      var tp = [];
                      var SI_SYMBOL = ['', 'k', 'M', 'B', 'T', 'Q'];
                      function abbreviateNumber(number){

                          // what tier? (determines SI symbol)
                          var tier = Math.log10(Math.abs(number)) / 3 | 0;

                          // if zero, we don't need a suffix
                          if(tier == 0) return number;

                          // get suffix and determine scale
                          var suffix = SI_SYMBOL[tier];
                          var scale = Math.pow(10, tier * 3);

                          // scale the number
                          var scaled = number / scale;

                          // format number and add suffix
                          return scaled.toFixed(1) + suffix;
                      };
                      params.forEach(function(x){
                          var estimate = abbreviateNumber(x.value[1]);
                          var mystring = x.seriesName + ': ' + estimate;
                          tp.push(mystring)
                      });
                      return(tp.join('<br/>'))
            }"
        )
    }
}