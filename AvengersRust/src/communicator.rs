#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        let mut ret_string = String::from("");

         let mut ret = match *self {

            Command::Invalid => 

            {ret_string.push_str("Not a command");
            ret_string.clone()
            
            },

            Command::Power(ref b, ref i) => 
            {ret_string.push_str("Power ");
            
            if *b {
                ret_string.push_str("increased by ");
            }else{
                ret_string.push_str("decreased by ");
            }
            ret_string.push_str(&(i.to_string()));
            ret_string.push_str("%");
            ret_string.clone()

            },

            Command::Missiles(ref b, ref i) => 
            {ret_string.push_str("Missiles ");
            
            if *b {
                ret_string.push_str("increased by ");
            }else{
                ret_string.push_str("decreased by ");
            }
            ret_string.push_str(&(i.to_string()));
            ret_string.clone()

            },

            Command::Shield(ref b) => 
            {ret_string.push_str("Shield turned ");
            
            if *b {
                ret_string.push_str("on");
            }else{
                ret_string.push_str("off");
            }
            ret_string.clone()
            },

            Command::Try => 
            {ret_string.push_str("Call attempt failed");
            ret_string.clone()
            
            },


            

        };
        ret

    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    let mut words: Vec<&str> = s.split(' ').collect();
    let mut num: i32 = 0;
    let mut ret = Command::Invalid;
    let mut len = words.len();
    
    if len == 2 {

        if words[0] == "shield" {
            if words[1] == "on"{
                ret = Command::Shield(true);
            }
            else if words[1] == "off"{
                ret = Command::Shield(false);
            }
        }

    }
    else if len == 3{

        if words[0] == "power" {
            num = words[2].parse::<i32>().unwrap();
            if words[1] == "inc"{
                ret = Command::Power(true, num);
            }
            else if words[1] == "dec"{
                ret = Command::Power(false, num);
            }
        }

          if words[2] == "missiles"{
            num = words[1].parse::<i32>().unwrap();
            if words[0] == "add"{
                ret = Command::Missiles(true, num);
            }
            else if words[0] == "fire"{
                ret = Command::Missiles(false, num);
            }
        }

    }
    else if len == 4{
        ret = Command::Try;
    }

    ret 
    
}
