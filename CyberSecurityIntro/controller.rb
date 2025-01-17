require "digest"
require "securerandom"
require "sqlite3"

#
# Constants
#

AVATAR_DIR = "./static/avatars"
SESSION_BITS = 32
DATE_FMT = "%d %h %G at %R"

#
# Helpers
#

# get_user: String -> (Hash or NilClass)
# 	Gets user preferences of given user or nil if non-existent
	def get_user_2(user, password)

		user = Rack::Utils.escape_html(user)

		query = %{
		SELECT Avatar, Description, Password
		FROM Users
		WHERE User = '#{user}' AND Password = '#{password}'
		}
		@db.execute(query) do |user|
			return {
				:avatar => user[0],
				:description => user[1]
			}
		end
		nil
	end



# upload_avatar : String File -> Boolean
# Uploads a file to the server (failing if it already exists)
# and returns success status.
def upload_avatar(filename, file)

	filename = Rack::Utils.escape_html(filename)

	path = "#{AVATAR_DIR}/#{filename}"
	return false if File.exists? path
	File.open(path, "wb") { |f| f.write(file.read) }
	true
end

#
# Sessions
#
# The Sessions module handles session identifiers, integers
# which uniquely identify a user on a particular client.
# Once issued, these identifiers will automatically be stored
# in a cookie on the user's browser. We keep track of which
# identifiers belong to whom.
#

module Sessions

	# issue_session : -> Integer
	# Returns cryptographically secure session identifier.
	def issue_session
		SecureRandom.random_number(2 ** SESSION_BITS)
	end

	# assign_session : String -> Integer
	# Assigns a session identifier to a user and returns it.
	def assign_session(user)
		user = Rack::Utils.escape_html(user)
		@sessions[user] = issue_session
		@sessions[user]
	end

	# revoke_session : String -> Integer
	# Revokes session of a user and returns it.
	def revoke_session(user)
		user = Rack::Utils.escape_html(user)
		# @tokens.delete user
		@sessions.delete user
	end
end

#
# Tokens
#
# The Tokens module generates and assigns tokens which are
# attached to user input forms as a hidden field. A new token
# should be assigned to a user on GET requests (where the
# page has a form).
#

module Tokens

	# issue_token : -> Integer
	# Returns cryptographically secure token.
	def issue_token
		SecureRandom.random_number(2 ** 16)
	end

	# assign_token : String -> Integer
	# Assigns a token identifier to a user and returns it.
	def assign_token(user)
		user = Rack::Utils.escape_html(user)
		@tokens[user] = issue_token
		@tokens[user]
	end
end

#
# Access
#
# The Access module handles user authentication and authorization.
# We verify the user's identity by matching up the session identifier
# the user gives us (as a cookie) to the identifier we issued for that
# user at login.
#

module Access

	# authenticate : String String -> (Integer or NilClass)
	# If credentials are valid, assigns session identifier to user
	# and returns identifier, otherwise returns nil.
	def authenticate(user, passwd)

		user = Rack::Utils.escape_html(user)
		password = Rack::Utils.escape_html(passwd)

		# hash_map = get_user_2(user, password) 

		# if hash_map then
		# 	hash = Digest::SHA256.hexdigest(password)  
		# 	if hash == hash_map[:password] then
		# 		return assign_session user
		# 	end
		# end
		# nil

		assign_session user if get_user_2(user, password)
	end

	# authorize : String Integer -> Boolean
	# Returns whether user was issued given session identifier.
	def authorize(user, session)

		user = Rack::Utils.escape_html(user)

		session == @sessions[user]
	end

	# revoke : String Integer -> (Integer or NilClass)
	# Revokes a user's session so long as given session identifier
	# is valid. Returns the session if valid, otherwise nil.
	def revoke(user, session)

		user = Rack::Utils.escape_html(user)

		revoke_session user if authorize(user, session)
	end
end

#
# User
#
# The User module handles all user-creating, modifying, and
# data retrieval actions.
#

module User

    # search : String -> Hash
    # Returns a hash containing the search query and the set of
    # users matching the query (by name or description).
    def search(user_query)

    	user_query = Rack::Utils.escape_html(user_query)
        users = []
       	query = @db.prepare(
                "SELECT User, Avatar, Description
                FROM Users
                WHERE User LIKE ? OR
		      Description LIKE ? ")
        e = query.execute([ '%' + user_query + '%', '%' + user_query + '%']) 
        e.each do |user|
                        users << {
                                :name => user[0],
                                :avatar => user[1],
                                :description => user[2]
                        }
                end
        { :query => user_query, :users => users }
    end

	# register : String String File String String -> Boolean
	# Registers a new user if they don't already exist and
	# password and confirm password match. Returns success status.
	def register(user, filename, file, password, confirm)
		user = Rack::Utils.escape_html(user)
		confirm = Rack::Utils.escape_html(confirm)
		password = Rack::Utils.escape_html(password)
		filename = Rack::Utils.escape_html(filename)

		return false if (password != confirm) || ((get_user user) != nil)

		upload_avatar(filename, file)
		secure_password = Digest::SHA256.hexdigest(password)
		query = @db.prepare("
		INSERT INTO Users(User, Password, Avatar)
		VALUES (?, ?, ?)")
		query.execute([user, secure_password, filename])
		true
	end

	# get_user: String -> (Hash or NilClass)
	# Gets user preferences of given user or nil if non-existent
	def get_user(user)

		query = %{
		SELECT Avatar, Description
		FROM Users
		WHERE User = '#{user}'
		}
		@db.execute(query) do |user|
			return {
				:avatar => user[0],
				:description => user[1]
			}
		end
		nil
	end

	# update_prefs : String Integer String Integer -> Boolean
	# Update preferences of given user returning success status.
	def update_prefs(user, session, description, token)

		user = Rack::Utils.escape_html(user)
		description = Rack::Utils.escape_html(description)

		return false if authorize(user, session) == false

		if token == @tokens[user] then

			query = @db.prepare("
			UPDATE Users
			SET Description = ?
			WHERE User = ?")
			query.execute([description, user])
			true

		end
		false
	end
end

#
# Posts
#
# The Posts module is concerned with creating and retrieving
# posts. These are the little messages that make up communication
# on our network.
#

module Posts

	# publish_post : String Integer String Integer -> Boolean
	# Publish post from user with given content. Returns
	# success status.
	def publish_post(user, session, content, token)

		user = Rack::Utils.escape_html(user)
		content = Rack::Utils.escape_html(content)

		return false if authorize(user, session) == false

		if token == @tokens[user] then

			timestamp = Time.now.to_i
			query = @db.prepare("
			INSERT INTO Posts(User, Content, Date)
			VALUES (?, ?, ?)")
			query.execute([user, content, timestamp])
			true
		end
		false
	end

	# get_posts : (String or NilClass) -> Array
	# Returns array of all posts from the given user or all posts
	# in the system if given nil.
	def get_posts(user)

		if user != nil
			user = Rack::Utils.escape_html(user)
		end

		posts = []
		query = %{
		SELECT Posts.User, Avatar, Content, Date
		FROM Posts
		JOIN Users ON Posts.User = Users.User
		#{"WHERE Posts.User = '#{user}'" if user}
		ORDER BY Posts.ID DESC
		}
		@db.execute(query) do |pos|
			date_str = Time.at(pos[3].to_i).strftime(DATE_FMT)
			posts << {
				:user => pos[0],
				:avatar => pos[1],
				:content => pos[2],
				:date => date_str
			}
		end
		posts
	end

	# all_posts : -> Array
	# Returns all posts.
	def all_posts
		get_posts nil
	end
end

#
# Controller
#
# The Controller class defines a single interface to all the
# previously defined modules. It holds the server-side state
# of the application as well as the database handle.
#

class Controller
	include Sessions
	include Tokens
	include Access
	include User
	include Posts

	# Leave @db as attr_accessor or you will fail the tests!
	attr_accessor :db

	def initialize
		@db = SQLite3::Database.new "data.db"
		@sessions = {}
		@tokens = {}
	end
end