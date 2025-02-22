import { useQuery, useMutation } from "@tanstack/react-query";
import {
  createUser,
  deleteUser,
  getUser,
  getUsers,
} from "../client/services/users";
import { CreateUserRequest, User } from "../__generated__/types.gen";

export const useGetUsers = () => {
  return useQuery<User[], Error>({
    queryKey: ["users"],
    queryFn: getUsers,
  });
};

export const useGetUser = (userid: string) => {
  return useQuery<User, Error>({
    queryKey: ["user", userid],
    queryFn: async () => await getUser(userid),
  });
};

export const useCreateUser = () => {
  return useMutation({
    mutationFn: (newUser: CreateUserRequest) => {
      return Promise.resolve(createUser(newUser));
    },
  });
};

export const useDeleteUser = () => {
  return useMutation({
    mutationFn: (userid: string) => {
      return Promise.resolve(deleteUser(userid));
    },
  });
};
